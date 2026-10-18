use std::io::{BufRead, Read, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{channel, Receiver};

pub type Value = i32;

pub const STACKSIZE: i32 = 256;
pub const MAXCELL: usize = 16383;
pub const MEMSIZE: usize = (MAXCELL + 1) * 4;
pub const MINCTRL: usize = MAXCELL - STACKSIZE as usize;
pub const MAXDATA: usize = MINCTRL - 1;
pub const MINDATA: usize = MAXDATA - STACKSIZE as usize;
pub const MINHEAP: Value = 256;
pub const DS_BASE: usize = MINDATA * 4;
pub const CS_BASE: usize = MINCTRL * 4;

pub const RGO: usize = (b'\\' - b'@') as usize;
pub const RLP: usize = (b'^' - b'@') as usize;
pub const RHP: usize = (b'_' - b'@') as usize;
pub const RIP: usize = 0x21;
pub const RDS: usize = 0x23;
pub const RCS: usize = 0x26;
pub const RST: usize = 0x28;
pub const RDB: usize = 0x29;
pub const RED: usize = 0x31;
pub const RBP: usize = 0x32;

pub const OPS: [(u8, &str); 46] = [
    (0x80, "ad"), (0x81, "sb"), (0x82, "ml"), (0x83, "dv"), (0x84, "md"), (0x85, "sh"),
    (0x86, "an"), (0x87, "or"), (0x88, "xr"), (0x89, "nt"), (0x8A, "eq"), (0x8B, "lt"),
    (0x8C, "du"), (0x8D, "sw"), (0x8E, "ov"), (0x8F, "zp"), (0x90, "dc"), (0x91, "cd"),
    (0x92, "rb"), (0x93, "ri"), (0x94, "wb"), (0x95, "wi"), (0x96, "lb"), (0x97, "li"),
    (0x98, "rs"), (0x99, "ls"), (0x9A, "jm"), (0x9B, "hp"), (0x9C, "h0"), (0x9D, "cl"),
    (0x9E, "rt"), (0x9F, "nx"), (0xA0, "fa"), (0xA1, "fs"), (0xA2, "fm"), (0xA3, "fd"),
    (0xA4, "fl"), (0xA5, "fi"), (0xBE, "tm"), (0xC0, "c0"), (0xC1, "c1"), (0xF6, "c2"),
    (0xF7, "n1"), (0xF8, "c4"), (0xFD, "io"), (0xFE, "db"),
];

pub fn opname(b: u8) -> String {
    match b {
        0 => "..".into(),
        0x01..=0x1F => format!("^{}", (b'@' + b) as char),
        0x20..=0x3F => format!("@{}", (b'@' + b - 0x20) as char),
        0x40..=0x5F => format!("!{}", (b'@' + b - 0x40) as char),
        0x60..=0x7F => format!("+{}", (b'@' + b - 0x60) as char),
        0xFF => "hl".into(),
        _ => OPS.iter().find(|o| o.0 == b).map_or(String::new(), |o| o.1.into()),
    }
}

pub fn regn(c: char) -> usize {
    (c.to_ascii_uppercase() as u32).wrapping_sub('@' as u32) as usize
}

pub fn rega(c: char) -> Value {
    4 * regn(c) as Value
}

pub fn b4mat(v: Value) -> String {
    if v < 0 { format!("-{:X}", (v as i64).unsigned_abs()) } else { format!("{v:X}") }
}

#[derive(Debug)]
pub struct Stop(pub i32);

pub type R<T> = Result<T, Stop>;

struct Proc {
    child: Child,
    stdin: Option<ChildStdin>,
    lines: Receiver<Vec<u8>>,
    outbuf: Vec<u8>,
    done: bool,
}

pub struct Vm {
    pub mem: Vec<u8>,
    pub ob: Vec<u8>,
    pub out: Box<dyn Write>,
    pub args: Vec<String>,
    procs: Vec<Option<Proc>>,
}

impl Vm {
    pub fn new(out: Box<dyn Write>) -> Vm {
        let mut vm = Vm { mem: vec![0; MEMSIZE], ob: vec![], out, args: std::env::args().collect(), procs: (0..16).map(|_| None).collect() };
        vm.boot();
        vm
    }

    pub fn boot(&mut self) {
        self.mem.iter_mut().for_each(|b| *b = 0);
        self.set_rg(RDS, 0);
        self.set_rg(RCS, 0);
        self.set_rg(RIP, MINHEAP);
        self.set_rg(RHP, MINHEAP);
        self.set_rg(RED, MINHEAP);
        self.set_rg(RBP, MAXCELL as Value);
        for p in self.procs.iter_mut() {
            if let Some(mut p) = p.take() {
                let _ = p.child.kill();
                let _ = p.child.wait();
            }
        }
    }

    fn raw_rd(&self, a: usize) -> Value {
        i32::from_le_bytes([self.mem[a], self.mem[a + 1], self.mem[a + 2], self.mem[a + 3]])
    }

    fn raw_wr(&mut self, a: usize, v: Value) {
        self.mem[a..a + 4].copy_from_slice(&v.to_le_bytes());
    }

    pub fn rg(&self, r: usize) -> Value {
        self.raw_rd(4 * r)
    }

    pub fn set_rg(&mut self, r: usize, v: Value) {
        self.raw_wr(4 * r, v)
    }

    pub fn say(&mut self, s: &str) {
        let _ = writeln!(self.out, "{s}");
    }

    pub fn write_stack(&self, pre: &str, base: usize, count: Value) -> String {
        let items: Vec<String> = (0..count.max(0) as usize).map(|i| b4mat(self.raw_rd(base + 4 * i))).collect();
        format!("{pre}[{}]", items.join(" "))
    }

    pub fn fail<T>(&mut self, msg: &str) -> R<T> {
        let ip = self.rg(RIP);
        let op = self.mem.get(ip as usize).copied().map(opname).unwrap_or_default();
        let ds = self.write_stack("ds: ", DS_BASE, self.rg(RDS));
        let cs = self.write_stack("cs: ", CS_BASE, self.rg(RCS));
        self.say(&format!("{msg}\nip: {ip} op: {op}\n{ds}\n{cs}"));
        Err(Stop(1))
    }

    fn addr(&mut self, a: Value, n: usize) -> R<usize> {
        if a < 0 || a as usize + n > MEMSIZE {
            return self.fail(&format!("address {} is outside ram (max: {:08X})", b4mat(a), MEMSIZE - 1));
        }
        Ok(a as usize)
    }

    pub fn rdval(&mut self, a: Value) -> R<Value> {
        if a < 0 || a as usize + 4 > MEMSIZE {
            return self.fail("read past mem(rdval)");
        }
        Ok(self.raw_rd(a as usize))
    }

    pub fn wrval(&mut self, a: Value, v: Value) -> R<()> {
        let a = self.addr(a, 4)?;
        self.raw_wr(a, v);
        Ok(())
    }

    pub fn bget(&mut self, a: Value) -> R<u8> {
        let a = self.addr(a, 1)?;
        Ok(self.mem[a])
    }

    pub fn bset(&mut self, a: Value, v: u8) -> R<()> {
        let a = self.addr(a, 1)?;
        self.mem[a] = v;
        Ok(())
    }

    pub fn dput(&mut self, v: Value) -> R<()> {
        let n = self.rg(RDS);
        if n >= STACKSIZE {
            return self.fail("overflow(ds)");
        }
        self.raw_wr(DS_BASE + 4 * n as usize, v);
        self.set_rg(RDS, n + 1);
        Ok(())
    }

    pub fn dpop(&mut self) -> R<Value> {
        let n = self.rg(RDS);
        if n <= 0 {
            return self.fail("underflow(ds)");
        }
        self.set_rg(RDS, n - 1);
        Ok(self.raw_rd(DS_BASE + 4 * (n - 1) as usize))
    }

    pub fn tos(&mut self) -> R<Value> {
        let n = self.rg(RDS);
        if n < 1 {
            return self.fail("underflow(tos)");
        }
        Ok(self.raw_rd(DS_BASE + 4 * (n - 1) as usize))
    }

    fn nos(&mut self) -> R<Value> {
        let n = self.rg(RDS);
        if n < 2 {
            return self.fail("underflow(nos)");
        }
        Ok(self.raw_rd(DS_BASE + 4 * (n - 2) as usize))
    }

    pub fn dswp(&mut self) -> R<()> {
        let a = self.dpop()?;
        let b = self.dpop()?;
        self.dput(a)?;
        self.dput(b)
    }

    pub fn cput(&mut self, v: Value) -> R<()> {
        let n = self.rg(RCS);
        if n >= STACKSIZE {
            return self.fail("overflow(cs)");
        }
        self.raw_wr(CS_BASE + 4 * n as usize, v);
        self.set_rg(RCS, n + 1);
        Ok(())
    }

    pub fn cpop(&mut self) -> R<Value> {
        let n = self.rg(RCS);
        if n <= 0 {
            return self.fail("underflow(cs)");
        }
        self.set_rg(RCS, n - 1);
        Ok(self.raw_rd(CS_BASE + 4 * (n - 1) as usize))
    }

    fn toc(&mut self) -> R<Value> {
        let n = self.rg(RCS);
        if n < 1 {
            return self.fail("underflow(toc)");
        }
        Ok(self.raw_rd(CS_BASE + 4 * (n - 1) as usize))
    }

    pub fn ds(&self) -> String {
        self.write_stack("ds: ", DS_BASE, self.rg(RDS))
    }

    pub fn cs(&self) -> String {
        self.write_stack("cs: ", CS_BASE, self.rg(RCS))
    }

    pub fn go(&mut self, a: Value) {
        self.set_rg(RIP, a.max(0x100) - 1)
    }

    fn hop(&mut self) -> R<()> {
        let ip = self.rg(RIP);
        let d = self.bget(ip + 1)? as i8 as Value;
        self.go(ip + d);
        Ok(())
    }

    fn cstr(&mut self, a: Value) -> R<String> {
        let mut s = vec![];
        let mut i = a;
        loop {
            let b = self.bget(i)?;
            if b == 0 {
                break;
            }
            s.push(b);
            i += 1;
        }
        Ok(String::from_utf8_lossy(&s).into_owned())
    }

    fn put_bytes(&mut self, a: Value, b: &[u8]) -> R<()> {
        for (i, &x) in b.iter().enumerate() {
            self.bset(a + i as Value, x)?;
        }
        Ok(())
    }

    fn pump(p: &mut Proc) {
        while let Ok(chunk) = p.lines.try_recv() {
            if chunk.is_empty() {
                p.done = true;
            } else {
                p.outbuf.extend(chunk);
            }
        }
    }

    fn line_ready(p: &mut Proc) -> bool {
        Self::pump(p);
        p.outbuf.contains(&b'\n') || (p.done || p.child.try_wait().ok().flatten().is_some()) && !p.outbuf.is_empty()
    }

    fn spawn(cmd: &str) -> Option<Proc> {
        let mut c = if cfg!(windows) {
            let mut c = Command::new("cmd.exe");
            c.args(["/c", cmd]);
            c
        } else {
            let mut c = Command::new("sh");
            c.args(["-c", cmd]);
            c
        };
        let mut child = c.stdin(Stdio::piped()).stdout(Stdio::piped()).stderr(Stdio::null()).spawn().ok()?;
        let mut stdout = child.stdout.take()?;
        let (tx, rx) = channel();
        std::thread::spawn(move || {
            let mut buf = [0u8; 4096];
            loop {
                match stdout.read(&mut buf) {
                    Ok(0) | Err(_) => {
                        let _ = tx.send(vec![]);
                        break;
                    }
                    Ok(n) => {
                        if tx.send(buf[..n].to_vec()).is_err() {
                            break;
                        }
                    }
                }
            }
        });
        let stdin = child.stdin.take();
        Some(Proc { child, stdin, lines: rx, outbuf: vec![], done: false })
    }

    fn opio(&mut self) -> R<()> {
        let cmd = self.dpop()? as u8 as char;
        match cmd {
            'e' => {
                let c = self.dpop()? as u8;
                self.ob.push(if c < b' ' { b' ' } else { c });
            }
            'o' => {
                let c = self.dpop()? as u8;
                let _ = self.out.write_all(&[c]);
                let _ = self.out.flush();
            }
            'd' => {
                let a = self.dpop()?;
                let f = self.cstr(a)?;
                let _ = std::fs::remove_file(f);
            }
            'x' => {
                let a = self.dpop()?;
                let f = self.cstr(a)?;
                self.dput(if std::path::Path::new(&f).is_file() { -1 } else { 0 })?;
            }
            'z' => {
                let a = self.dpop()?;
                let f = self.cstr(a)?;
                self.dput(std::fs::metadata(&f).map_or(-1, |m| m.len() as Value))?;
            }
            'm' => {
                let a = self.dpop()?;
                let f = self.cstr(a)?;
                let dest = self.dpop()?;
                if let Ok(bytes) = std::fs::read(&f) {
                    self.put_bytes(dest, &bytes)?;
                }
            }
            's' => {
                let fa = self.dpop()?;
                let len = self.dpop()?;
                let src = self.dpop()?;
                let f = self.cstr(fa)?;
                let mut bytes = vec![];
                for i in 0..len.max(0) {
                    bytes.push(self.bget(src + i)?);
                }
                let _ = std::fs::write(f, bytes);
            }
            'p' => {
                let a = self.dpop()?;
                let cmd = self.cstr(a)?;
                match self.procs.iter().position(Option::is_none) {
                    None => self.dput(-1)?,
                    Some(i) => match Self::spawn(&cmd) {
                        Some(p) => {
                            self.procs[i] = Some(p);
                            self.dput(i as Value)?;
                        }
                        None => self.dput(-1)?,
                    },
                }
            }
            'w' => {
                let h = self.dpop()?;
                let a = self.dpop()?;
                if (0..16).contains(&h) && self.procs[h as usize].is_some() {
                    let mut line = self.cstr(a)?.into_bytes();
                    line.push(b'\n');
                    if let Some(Some(p)) = self.procs.get_mut(h as usize) {
                        if let Some(i) = p.stdin.as_mut() {
                            let _ = i.write_all(&line);
                            let _ = i.flush();
                        }
                    }
                }
            }
            'l' => {
                let h = self.dpop()?;
                let ready = match self.procs.get_mut(h.max(0) as usize).filter(|_| (0..16).contains(&h)) {
                    Some(Some(p)) => Self::line_ready(p),
                    _ => true,
                };
                self.dput(if ready { -1 } else { 0 })?;
            }
            'r' => {
                let h = self.dpop()?;
                let dest = self.dpop()?;
                let line = match self.procs.get_mut(h.max(0) as usize).filter(|_| (0..16).contains(&h)) {
                    Some(Some(p)) => {
                        while !Self::line_ready(p) {
                            if p.done && p.outbuf.is_empty() {
                                break;
                            }
                            std::thread::sleep(std::time::Duration::from_millis(1));
                        }
                        Some(if let Some(nl) = p.outbuf.iter().position(|&b| b == b'\n') {
                            let mut l: Vec<u8> = p.outbuf.drain(..=nl).collect();
                            l.pop();
                            if l.last() == Some(&b'\r') {
                                l.pop();
                            }
                            l
                        } else {
                            std::mem::take(&mut p.outbuf)
                        })
                    }
                    _ => None,
                };
                match line {
                    Some(l) => {
                        self.put_bytes(dest, &l)?;
                        self.bset(dest + l.len() as Value, 0)?;
                        self.dput(l.len() as Value)?;
                    }
                    None => self.dput(-1)?,
                }
            }
            'k' => {
                let h = self.dpop()?;
                if (0..16).contains(&h) {
                    if let Some(mut p) = self.procs[h as usize].take() {
                        let _ = p.child.kill();
                        let _ = p.child.wait();
                    }
                }
            }
            'i' => {
                let len = self.dpop()?;
                let dest = self.dpop()?;
                let mut line = String::new();
                if std::io::stdin().lock().read_line(&mut line).unwrap_or(0) == 0 {
                    self.bset(dest, 0)?;
                    return self.dput(0);
                }
                let mut b = line.trim_end_matches('\n').trim_end_matches('\r').as_bytes().to_vec();
                b.truncate(len.max(0) as usize);
                self.put_bytes(dest, &b)?;
                self.bset(dest + b.len() as Value, 0)?;
                self.dput(b.len() as Value)?;
            }
            'A' => self.dput(self.args.len() as Value - 1)?,
            'a' => {
                let i = self.dpop()?;
                let dest = self.dpop()?;
                match self.args.get(i.max(0) as usize).filter(|_| i >= 0).cloned() {
                    Some(arg) => {
                        let mut b = arg.into_bytes();
                        b.truncate(255);
                        self.bset(dest, b.len() as u8)?;
                        self.put_bytes(dest + 1, &b)?;
                    }
                    None => self.bset(dest, 0)?,
                }
            }
            'E' => {
                let dest = self.dpop()?;
                let na = self.dpop()?;
                let name = self.cstr(na)?;
                let mut b = std::env::var(name).unwrap_or_default().into_bytes();
                b.truncate(255);
                self.bset(dest, b.len() as u8)?;
                self.put_bytes(dest + 1, &b)?;
            }
            'S' => {
                let ms = self.dpop()?;
                if ms > 0 {
                    std::thread::sleep(std::time::Duration::from_millis(ms as u64));
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn f32bin(a: Value, b: Value, which: u8) -> Value {
        let (x, y) = (f32::from_bits(a as u32), f32::from_bits(b as u32));
        let r = match which {
            0 => x + y,
            1 => x - y,
            2 => x * y,
            _ => x / y,
        };
        r.to_bits() as Value
    }

    pub fn runop(&mut self, op: u8) -> R<()> {
        let ip = self.rg(RIP);
        match op {
            0 => {}
            0x01..=0x1F => {
                self.cput(ip + 1)?;
                let a = self.rg(op as usize);
                self.go(a);
            }
            0x20..=0x3F => self.dput(self.rg((op % 32) as usize))?,
            0x40..=0x5F => {
                let v = self.dpop()?;
                self.set_rg((op % 32) as usize, v);
            }
            0x60..=0x7F => {
                let r = (op % 32) as usize;
                let v = self.rg(r);
                let d = self.dpop()?;
                self.set_rg(r, v.wrapping_add(d));
                self.dput(v)?;
            }
            0x80 => {
                let v = self.dpop()?.wrapping_add(self.dpop()?);
                self.dput(v)?
            }
            0x81 => {
                let t = self.dpop()?;
                let v = self.dpop()?.wrapping_sub(t);
                self.dput(v)?
            }
            0x82 => {
                let v = self.dpop()?.wrapping_mul(self.dpop()?);
                self.dput(v)?
            }
            0x83 | 0x84 => {
                let t = self.dpop()?;
                let x = self.dpop()?;
                if t == 0 {
                    return self.fail("division by zero");
                }
                self.dput(if op == 0x83 { x.wrapping_div(t) } else { x.wrapping_rem(t) })?
            }
            0x85 => {
                let t = self.dpop()?;
                let x = self.dpop()?;
                self.dput(if t < 0 { (x as u32).wrapping_shr(t.wrapping_neg() as u32) as Value } else { x.wrapping_shl(t as u32) })?
            }
            0x86 => {
                let v = self.dpop()? & self.dpop()?;
                self.dput(v)?
            }
            0x87 => {
                let v = self.dpop()? | self.dpop()?;
                self.dput(v)?
            }
            0x88 => {
                let v = self.dpop()? ^ self.dpop()?;
                self.dput(v)?
            }
            0x89 => {
                let v = !self.dpop()?;
                self.dput(v)?
            }
            0x8A => {
                let v = self.dpop()? == self.dpop()?;
                self.dput(-(v as Value))?
            }
            0x8B => {
                let t = self.dpop()?;
                let v = self.dpop()? < t;
                self.dput(-(v as Value))?
            }
            0x8C => {
                let v = self.tos()?;
                self.dput(v)?
            }
            0x8D => self.dswp()?,
            0x8E => {
                let v = self.nos()?;
                self.dput(v)?
            }
            0x8F => {
                self.dpop()?;
            }
            0x90 => {
                let v = self.dpop()?;
                self.cput(v)?
            }
            0x91 => {
                let v = self.cpop()?;
                self.dput(v)?
            }
            0x92 => {
                let a = self.dpop()?;
                let v = self.bget(a)? as Value;
                self.dput(v)?
            }
            0x93 => {
                let a = self.dpop()?;
                let v = self.rdval(a)?;
                self.dput(v)?
            }
            0x94 => {
                let a = self.dpop()?;
                let v = self.dpop()?;
                self.bset(a, v as u8)?
            }
            0x95 => {
                let a = self.dpop()?;
                let v = self.dpop()?;
                self.wrval(a, v)?
            }
            0x96 => {
                let v = self.bget(ip + 1)? as Value;
                self.dput(v)?;
                self.set_rg(RIP, ip + 1);
            }
            0x97 => {
                let v = self.rdval(ip + 1)?;
                self.dput(v)?;
                self.set_rg(RIP, ip + 4);
            }
            0x98 => {
                let a = self.dpop()?;
                let v = self.bget(a)? as i8 as Value;
                self.dput(v)?
            }
            0x99 => {
                let v = self.bget(ip + 1)? as i8 as Value;
                self.dput(v)?;
                self.set_rg(RIP, ip + 1);
            }
            0x9A => {
                let a = self.rdval(ip + 1)?;
                self.go(a)
            }
            0x9B => self.hop()?,
            0x9C => {
                if self.dpop()? == 0 {
                    self.hop()?
                } else {
                    self.set_rg(RIP, ip + 1)
                }
            }
            0x9D => {
                self.cput(ip + 5)?;
                let a = self.rdval(ip + 1)?;
                self.go(a)
            }
            0x9E => {
                let a = self.cpop()? - 1;
                self.set_rg(RIP, a);
                if a == -1 {
                    self.set_rg(RST, 0)
                }
            }
            0x9F => {
                if self.toc()? > 0 {
                    let t = self.cpop()?;
                    self.cput(t - 1)?;
                }
                if self.toc()? == 0 {
                    self.cpop()?;
                    self.set_rg(RIP, self.rg(RIP) + 1);
                } else {
                    self.hop()?
                }
            }
            0xA0..=0xA3 => {
                let t = self.dpop()?;
                let a = self.dpop()?;
                self.dput(Self::f32bin(a, t, op - 0xA0))?
            }
            0xA4 => {
                let t = self.dpop()?;
                let a = self.dpop()?;
                self.dput(if f32::from_bits(a as u32) < f32::from_bits(t as u32) { -1 } else { 0 })?
            }
            0xA5 => {
                let a = self.dpop()?;
                self.dput((a as f32).to_bits() as Value)?
            }
            0xBE => {
                if self.dpop()? as u8 == b'e' {
                    let _ = self.out.write_all(b"e");
                }
            }
            0xC0 => self.dput(0)?,
            0xC1 => self.dput(1)?,
            0xF6 => self.dput(2)?,
            0xF7 => self.dput(-1)?,
            0xF8 => self.dput(4)?,
            0xFD => self.opio()?,
            0xFE => self.set_rg(RDB, 1),
            0xFF => self.set_rg(RST, 0),
            _ => {}
        }
        Ok(())
    }

    pub fn step(&mut self) -> R<Value> {
        let ip = self.rg(RIP);
        let op = self.bget(ip)?;
        self.runop(op)?;
        let ip = self.rg(RIP) + 1;
        self.set_rg(RIP, ip);
        Ok(ip)
    }
}
