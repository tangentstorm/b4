use std::io::Write;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn b4() -> b4::B4 {
    b4::B4::new(Box::new(std::io::sink()))
}

#[test]
fn io_i_reads_the_next_stdin_line_while_b4i_is_reading_commands() {
    let mut c = Command::new(env!("CARGO_BIN_EXE_b4i")).stdin(Stdio::piped()).stdout(Stdio::piped()).spawn().unwrap();
    c.stdin.take().unwrap().write_all(b"200 10 'i io ?d\nhello\n?200 /q\n").unwrap();
    let end = Instant::now() + Duration::from_secs(10);
    while c.try_wait().unwrap().is_none() {
        if Instant::now() > end {
            c.kill().unwrap();
            panic!("b4i hung on io 'i'");
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    let out = String::from_utf8(c.wait_with_output().unwrap().stdout).unwrap();
    assert_eq!(out.lines().next(), Some("ds: [5]"), "{out}");
    assert!(out.lines().nth(1).unwrap().starts_with("+H +E +L +L +O .."), "{out}");
}

#[test]
fn the_data_and_control_stacks_do_not_overlap() {
    let mut b = b4();
    for i in 0..200 {
        b.vm.dput(i).unwrap();
    }
    for i in 0..200 {
        b.vm.cput(-i).unwrap();
    }
    for i in (0..200).rev() {
        assert_eq!(b.vm.cpop().unwrap(), -i);
    }
    for i in (0..200).rev() {
        assert_eq!(b.vm.dpop().unwrap(), i);
    }
}

#[test]
fn ram_is_the_whole_64k() {
    let mut b = b4();
    assert_eq!(b.vm.mem.len(), 0x10000);
    b.vm.bset(0xFFFF, 7).unwrap();
    assert_eq!(b.vm.bget(0xFFFF).unwrap(), 7);
    b.vm.wrval(0xFFFC, 0x01020304).unwrap();
    assert_eq!(b.vm.rdval(0xFFFC).unwrap(), 0x01020304);
    assert!(b.vm.bget(0x10000).is_err());
    let img = std::path::Path::new(env!("CARGO_TARGET_TMPDIR")).join("full.b4x");
    let mut bytes = vec![0u8; 0x10000];
    bytes[0xFFFF] = 0xAB;
    std::fs::write(&img, bytes).unwrap();
    let mut b = b4();
    b.args(&["-l".into(), img.to_str().unwrap().into()]).unwrap();
    assert_eq!(b.vm.mem[0xFFFF], 0xAB);
}

#[test]
fn clear_drops_pending_forward_references() {
    let mut b = b4();
    b.line(":100 >later").unwrap();
    b.line("/C").unwrap();
    b.line(":100 AA BB CC DD").unwrap();
    b.line(":later").unwrap();
    assert_eq!(&b.vm.mem[0x100..0x104], &[0xAA, 0xBB, 0xCC, 0xDD]);
}

#[test]
fn each_stack_holds_256_cells() {
    let mut b = b4();
    for i in 0..256 {
        b.vm.dput(i).unwrap();
        b.vm.cput(i).unwrap();
    }
    assert!(b.vm.dput(256).is_err());
    assert!(b.vm.cput(256).is_err());
    for i in (0..256).rev() {
        assert_eq!(b.vm.dpop().unwrap(), i);
        assert_eq!(b.vm.cpop().unwrap(), i);
    }
}

#[test]
fn a_two_character_query_that_is_not_a_register_is_a_memory_query() {
    let mut b = b4();
    b.vm.mem[0] = 0x80;
    assert!(b.line("?0").is_ok());
    assert!(b.line("?a ?_").is_ok());
}

#[cfg(target_os = "linux")]
#[test]
fn clear_kills_the_processes_io_p_started() {
    let me = std::process::id().to_string();
    let children = || -> Vec<u32> {
        std::fs::read_dir("/proc")
            .unwrap()
            .flatten()
            .filter_map(|e| e.file_name().to_str()?.parse::<u32>().ok())
            .filter(|p| {
                let stat = std::fs::read_to_string(format!("/proc/{p}/stat")).unwrap_or_default();
                let cmd = std::fs::read(format!("/proc/{p}/cmdline")).unwrap_or_default();
                stat.rsplit(')').next().and_then(|r| r.split_whitespace().nth(1)) == Some(me.as_str())
                    && String::from_utf8_lossy(&cmd).contains("b4-clear-test")
            })
            .collect()
    };
    let mut b = b4();
    b.line(":300 \"sleep 30 # b4-clear-test\" 0").unwrap();
    b.line("300 'p io").unwrap();
    assert_eq!(b.vm.dpop().unwrap(), 0);
    let kids = children();
    assert!(!kids.is_empty(), "io 'p' started nothing");
    b.line("/C").unwrap();
    for k in kids {
        assert!(!std::path::Path::new(&format!("/proc/{k}")).exists(), "/C left child {k} running");
    }
}
