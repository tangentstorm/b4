use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

#[derive(Clone, Default)]
struct Out(Rc<RefCell<Vec<u8>>>);

impl Write for Out {
    fn write(&mut self, b: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().extend_from_slice(b);
        Ok(b.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn b4() -> (b4::B4, Out) {
    let out = Out::default();
    let mut b = b4::B4::new(Box::new(out.clone()));
    b.vm.add_op(0xA6, "dd", |vm| {
        let x = vm.dpop()?;
        vm.dput(x * 2)
    })
    .unwrap();
    (b, out)
}

#[test]
fn a_custom_op_runs_from_the_command_line_and_from_assembled_code() {
    let (mut b, _) = b4();
    b.line("5 dd").unwrap();
    assert_eq!(b.vm.dpop().unwrap(), 10);
    b.line(":f lb 07 dd rt").unwrap();
    b.line("f").unwrap();
    assert_eq!(b.vm.dpop().unwrap(), 14);
    assert_eq!(b.vm.mem[0x102], 0xA6);
}

#[test]
fn the_debugger_shows_a_custom_op_by_its_name() {
    let (mut b, out) = b4();
    b.line(":100 lb 07 dd rt").unwrap();
    b.line("?100").unwrap();
    let text = String::from_utf8(out.0.borrow().clone()).unwrap();
    assert!(text.starts_with("lb ^G dd rt .."), "{text}");
}

#[test]
fn a_custom_op_can_stop_the_vm_with_an_error() {
    let (mut b, _) = b4();
    b.vm.add_op(0xA7, "xx", |vm| vm.fail("custom failure")).unwrap();
    assert!(b.line("xx").is_err());
}

#[test]
fn built_in_bytes_and_names_cannot_be_taken() {
    let (mut b, _) = b4();
    assert!(b.vm.add_op(0x80, "zz", |_| Ok(())).is_err());
    assert!(b.vm.add_op(0x20, "zz", |_| Ok(())).is_err());
    assert!(b.vm.add_op(0xA8, "ad", |_| Ok(())).is_err());
    assert!(b.vm.add_op(0xA8, "dd", |_| Ok(())).is_err());
    assert!(b.vm.add_op(0xA8, "toolong", |_| Ok(())).is_err());
}

#[test]
fn names_are_two_lower_case_letters_not_already_in_use() {
    let (mut b, _) = b4();
    for bad in ["AA", "Ab", "a1", "a_", "z", "abc", "é", ""] {
        assert!(b.vm.add_op(0xA8, bad, |_| Ok(())).is_err(), "{bad:?} was accepted");
    }
    assert!(b.vm.add_op(0xA8, "dd", |_| Ok(())).is_err(), "a name already in use was accepted");
    assert!(b.vm.add_op(0xA6, "zz", |_| Ok(())).is_err(), "a byte already in use was taken again");
    assert!(b.vm.add_op(0xA8, "zz", |vm| vm.dput(1)).is_ok());
    b.line("zz").unwrap();
    assert_eq!(b.vm.dpop().unwrap(), 1);
}

#[test]
fn an_unregistered_free_byte_is_still_a_no_op() {
    let (mut b, _) = b4();
    b.line(":g lb 03 B0 rt").unwrap();
    b.line("g").unwrap();
    assert_eq!(b.vm.dpop().unwrap(), 3);
}
