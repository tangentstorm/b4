use crate::asm::{b4op, is_reg_char, try_hex, Asm, Dict};
use crate::vm::{b4mat, opname, rega, regn, Value, Vm, R, RCS, RDB, RDS, RED, RHP, RIP, RST};
use std::io::Write;

pub struct B4 {
    pub vm: Vm,
    pub dict: Dict,
    pub b4h_mode: bool,
    pub logging: bool,
    saved_here: Value,
    on_semi: bool,
}

pub fn tokenize(line: &str) -> Vec<String> {
    let mut toks = vec![];
    let mut cur = String::new();
    let mut in_str = false;
    let mut in_cstr = false;
    let c: Vec<char> = line.chars().collect();
    let mut i = 0;
    while i < c.len() {
        if c[i] == '#' && !in_str && !in_cstr {
            break;
        }
        if in_str || in_cstr {
            cur.push(c[i]);
            if c[i] == '"' {
                toks.push(std::mem::take(&mut cur));
                in_str = false;
                in_cstr = false;
            }
        } else {
            match c[i] {
                ' ' | '\t' | '\r' | '\n' => {
                    if !cur.is_empty() {
                        toks.push(std::mem::take(&mut cur));
                    }
                }
                '.' if c.get(i + 1) == Some(&'"') => {
                    if !cur.is_empty() {
                        toks.push(std::mem::take(&mut cur));
                    }
                    cur = ".\"".into();
                    in_cstr = true;
                    i += 1;
                }
                '"' => {
                    if !cur.is_empty() {
                        toks.push(std::mem::take(&mut cur));
                    }
                    cur = "\"".into();
                    in_str = true;
                }
                ch => cur.push(ch),
            }
        }
        i += 1;
    }
    if !cur.is_empty() {
        toks.push(cur);
    }
    toks
}

impl B4 {
    pub fn new(out: Box<dyn Write>) -> B4 {
        let mut vm = Vm::new(out);
        vm.set_rg(RIP, 0x100);
        vm.set_rg(RHP, 0x100);
        B4 { vm, dict: Dict::default(), b4h_mode: false, logging: false, saved_here: 0, on_semi: false }
    }

    fn asm(&mut self) -> Asm<'_> {
        Asm { vm: &mut self.vm, dict: &mut self.dict }
    }

    fn say(&mut self, s: &str) {
        self.vm.say(s)
    }

    fn log_state(&mut self, op: &str) {
        if self.logging {
            let a = self.vm.rg(RIP);
            let id = self.dict.ents.iter().rev().find(|e| e.1 == a).map(|e| format!("{{ {} }} ", e.0)).unwrap_or_default();
            let (ds, cs) = (self.vm.ds(), self.vm.cs());
            self.say(&format!("| IP: {} {id}OP: {op}  {ds}  {cs}", b4mat(a)));
        }
    }

    fn wrap_op(&mut self, op: u8) -> R<()> {
        self.log_state(&opname(op));
        self.vm.runop(op)
    }

    fn step_wrapped(&mut self) -> R<()> {
        let ip = self.vm.rg(RIP);
        let op = self.vm.bget(ip)?;
        self.wrap_op(op)?;
        self.vm.set_rg(RIP, self.vm.rg(RIP) + 1);
        Ok(())
    }

    fn run_wrapped(&mut self) -> R<()> {
        self.vm.set_rg(RST, 1);
        self.vm.set_rg(RDB, 0);
        while self.vm.rg(RST) == 1 && self.vm.rg(RDB) == 0 {
            self.step_wrapped()?;
        }
        Ok(())
    }

    fn wrap_call(&mut self, addr: Value) -> R<()> {
        let ip = self.vm.rg(RIP);
        self.vm.cput(ip)?;
        self.vm.cput(0)?;
        self.vm.go(addr + 1);
        self.run_wrapped()?;
        if self.vm.rg(RDB) == 0 {
            let ip = self.vm.cpop()?;
            self.vm.set_rg(RIP, ip);
        }
        Ok(())
    }

    fn show_mem(&mut self, a: Value) -> R<()> {
        let mut s = String::new();
        for i in 0..16 {
            let v = self.vm.bget(a + i)?;
            let t = opname(v);
            s.push_str(if t.is_empty() { format!("{v:02X}") } else { t }.as_str());
            s.push(' ');
        }
        self.say(&s);
        Ok(())
    }

    fn show_hex_mem(&mut self, a: Value) -> R<()> {
        let mut b = vec![];
        for i in 0..16 {
            b.push(format!("{:02X}", self.vm.bget(a + i)?));
        }
        self.say(&b.join(" "));
        Ok(())
    }

    fn drain_output_hex(&mut self) {
        let ob = std::mem::take(&mut self.vm.ob);
        self.say(&format!("{:02X}", ob.len() & 0xFF));
        for chunk in ob.chunks(16) {
            self.say(&chunk.iter().map(|b| format!("{b:02X}")).collect::<Vec<_>>().join(" "));
        }
    }

    fn show_regs(&mut self) {
        let mut s = String::new();
        for (i, r) in "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_".chars().enumerate() {
            s.push_str(&format!("  {r}: {:08X}", self.vm.rg(regn(r))));
            if (i + 1) % 4 == 0 {
                s.push('\n');
            }
        }
        let _ = write!(self.vm.out, "{s}");
    }

    fn print_words(&mut self) {
        let mut s = String::new();
        for (i, (id, a)) in self.dict.ents.iter().enumerate() {
            s.push_str(&format!("{:04X}:{id:<11}", a));
            if i > 0 && i % 5 == 4 {
                s.push('\n');
            }
        }
        self.say(&s);
    }

    fn show_opcodes(&mut self) {
        let mut s = String::from("    ");
        for col in 0..16 {
            s.push_str(&format!(" +{col:X}"));
        }
        s.push_str("\n   +------------------------------------------------\n");
        for row in 0..16u8 {
            s.push_str(&format!("${:02X}| ", row * 16));
            for col in 0..16u8 {
                let t = opname(row * 16 + col);
                s.push_str(&format!("{:<3}", if t.is_empty() { "  ".to_owned() } else { t }));
            }
            s.push('\n');
        }
        let _ = write!(self.vm.out, "{s}");
    }

    fn help(&mut self) {
        for l in [
            "-- b4i: interactive interpreter for b4 virtual machine",
            "",
            "enter b4a assembly language tokens to execute immediately",
            "",
            "use :$hex-addr (ex: \":100\") to assemble rest of line",
            "use :$reg-name (ex: \":E\") to assemble line and assign register",
            "",
            "example:",
            "",
            "  :E lb 'e io rt                        # emit character",
            "  :W !R c1 +R rb .f c1 +R rb ^E .n rt   # write counted string",
            "  :S 05 'h 'e 'l 'l 'o                  # counted string",
            "  @S ^W                                 # (W)rite the (S)tring",
            "",
            "for help, see https://github.com/tangentstorm/b4/tree/main/doc",
            "",
        ] {
            self.say(l);
        }
    }

    pub fn file(&mut self, path: &str) -> R<bool> {
        let Ok(text) = std::fs::read_to_string(path) else {
            self.say(&format!("file not found: {path}"));
            return Ok(false);
        };
        for line in text.lines() {
            if self.line(line)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn colon(&mut self, tok: &str) -> R<()> {
        let w: String = tok.chars().skip(1).collect();
        if w.is_empty() {
            return Ok(());
        }
        if w.len() > 1 && w.starts_with('!') {
            let name = &w[1..];
            match self.dict.find(name) {
                Some(a) => {
                    self.saved_here = self.vm.rg(RHP);
                    self.vm.set_rg(RHP, a);
                    self.on_semi = true;
                }
                None => self.say(&format!("unknown label: {name}")),
            }
        } else if let Some(r) = (w.chars().count() == 1).then(|| is_reg_char(w.chars().next().unwrap())).flatten() {
            let h = self.vm.rg(RHP);
            self.vm.set_rg(r, h);
        } else if let Some(a) = try_hex(&w) {
            self.vm.set_rg(RHP, a);
        } else {
            self.asm().line(tok)?;
        }
        Ok(())
    }

    fn reg_or_label(&mut self, rest: &str) -> Option<Result<usize, Value>> {
        let c: Vec<char> = rest.chars().collect();
        if c.len() == 1 {
            if let Some(r) = is_reg_char(c[0]) {
                return Some(Ok(r));
            }
        }
        self.dict.find(rest).map(Err)
    }

    pub fn line(&mut self, line: &str) -> R<bool> {
        let mut done = false;
        if line.is_empty() {
            return Ok(false);
        }
        let toks = tokenize(line);
        let mut asm_mode = false;
        let mut i = 0;
        while i < toks.len() {
            let tok = toks[i].clone();
            i += 1;
            if tok.is_empty() {
                continue;
            }
            if tok.starts_with('#') {
                break;
            }
            if asm_mode {
                if tok == ";" {
                    asm_mode = false;
                    if self.on_semi {
                        self.vm.set_rg(RHP, self.saved_here);
                        self.on_semi = false;
                    }
                } else if tok.starts_with(':') {
                    self.colon(&tok)?;
                } else {
                    self.asm().line(&tok)?;
                }
                continue;
            }
            let arg = toks.get(i).cloned();
            match tok.as_str() {
                "/a" => match arg {
                    Some(a) => {
                        i += 1;
                        self.asm().file(&a)?
                    }
                    None => self.say("usage: /a <filename>"),
                },
                "/C" => {
                    self.vm.boot();
                    self.dict.clear();
                }
                "/d" => match arg {
                    Some(a) => {
                        i += 1;
                        if std::env::set_current_dir(&a).is_err() {
                            self.say(&format!("error changing directory to {a}"));
                        }
                    }
                    None => {
                        let d = std::env::current_dir().map(|d| d.display().to_string()).unwrap_or_default();
                        self.say(&d)
                    }
                },
                "/f" => {
                    let lines: Vec<String> = self.dict.fwds.iter().map(|f| format!("{:04X}>{}", f.at, f.key)).collect();
                    for l in lines {
                        self.say(&l);
                    }
                }
                "/g" | "//" => self.run_wrapped()?,
                "/j" => match arg {
                    Some(a) => {
                        i += 1;
                        if let Some(v) = try_hex(&a) {
                            self.vm.set_rg(RIP, v);
                        }
                    }
                    None => self.say("usage: /j <address>"),
                },
                "/h" => self.help(),
                "/i" => match arg {
                    Some(a) => {
                        i += 1;
                        self.file(&a)?;
                    }
                    None => self.say("usage: /i <filename>"),
                },
                "/o" | "-o" => self.show_opcodes(),
                "/ox?" => self.drain_output_hex(),
                "/p" => self.print_words(),
                "/q" => done = true,
                "/R" => {
                    self.vm.set_rg(RCS, 0);
                    self.vm.set_rg(RDS, 0);
                    self.vm.set_rg(RIP, 0x100);
                }
                "/" | "/s" => {
                    self.vm.step()?;
                }
                "/t" => self.logging = !self.logging,
                "/v" => match arg.as_deref().map(try_hex) {
                    Some(Some(v)) => {
                        i += 1;
                        self.vm.set_rg(RED, v)
                    }
                    Some(None) => {
                        i += 1;
                        self.say("usage: /v <address>")
                    }
                    None => self.vm.set_rg(RED, self.vm.rg(RIP)),
                },
                "?d" => {
                    let s = self.vm.ds();
                    self.say(&s)
                }
                "?c" => {
                    let s = self.vm.cs();
                    self.say(&s)
                }
                "?i" => {
                    let s = format!("ip: {}", b4mat(self.vm.rg(RIP)));
                    self.say(&s)
                }
                "?r" => self.show_regs(),
                _ => self.other(&tok)?,
            }
            if done {
                break;
            }
            if tok.starts_with(':') {
                asm_mode = true;
            }
        }
        if !self.vm.ob.is_empty() && !self.b4h_mode {
            let ob = std::mem::take(&mut self.vm.ob);
            let _ = self.vm.out.write_all(&ob);
            let _ = self.vm.out.write_all(b"\n");
        }
        let _ = self.vm.out.flush();
        Ok(done)
    }

    fn other(&mut self, tok: &str) -> R<()> {
        let c: Vec<char> = tok.chars().collect();
        let rest: String = c[1..].iter().collect();
        match c[0] {
            '\'' => self.vm.dput(if c.len() == 1 { 32 } else { c[1] as Value })?,
            '`' => match self.reg_or_label(&rest) {
                Some(Ok(_)) => self.vm.dput(rega(c[1]))?,
                Some(Err(a)) => self.vm.dput(a)?,
                None => return self.err("unknown `word"),
            },
            '@' => match self.reg_or_label(&rest) {
                Some(Ok(r)) => self.vm.dput(self.vm.rg(r))?,
                Some(Err(a)) => {
                    let v = self.vm.rdval(a)?;
                    self.vm.dput(v)?
                }
                None => return self.err("unknown @word"),
            },
            '!' => match self.reg_or_label(&rest) {
                Some(Ok(r)) => {
                    let v = self.vm.dpop()?;
                    self.vm.set_rg(r, v)
                }
                Some(Err(a)) => {
                    let v = self.vm.dpop()?;
                    self.vm.wrval(a, v)?
                }
                None => return self.err("unknown !word"),
            },
            '?' => {
                if c.len() == 2 && is_reg_char(c[1].to_ascii_uppercase()).is_some() {
                    let s = format!("{:08X}", self.vm.rg(regn(c[1])));
                    self.say(&s);
                } else if c.len() == 3 && c[1] == '@' {
                    let a = self.vm.rdval(rega(c[2]))?;
                    let _ = write!(self.vm.out, "{a:08X} ");
                    self.show_mem(a)?;
                } else if let Some(a) = rest.strip_suffix('x').filter(|r| !r.is_empty()).and_then(try_hex) {
                    self.show_hex_mem(a)?;
                } else if let Some(a) = try_hex(&rest) {
                    self.show_mem(a)?;
                } else if let Some(a) = self.dict.find(&rest) {
                    let _ = write!(self.vm.out, "{a:08X} ");
                    self.show_mem(a)?;
                } else {
                    self.say(&format!("invalid address: {rest}"));
                }
            }
            ':' => self.colon(tok)?,
            _ => {
                if c.len() > 1 && c[0] == '/' && try_hex(&rest).is_some() {
                    self.vm.set_rg(RIP, try_hex(&rest).unwrap());
                } else if let Some(v) = try_hex(tok) {
                    self.vm.dput(v)?;
                } else if let Some(a) = self.dict.find(tok) {
                    self.wrap_call(a)?;
                } else if let Some(op) = b4op(tok) {
                    if op < 0x20 {
                        let a = self.vm.rg(op as usize);
                        self.wrap_call(a)?;
                    } else {
                        self.wrap_op(op)?;
                    }
                } else {
                    self.say(&format!("what does \"{tok}\" mean?"));
                }
            }
        }
        Ok(())
    }

    fn err(&mut self, s: &str) -> R<()> {
        self.say(s);
        Err(crate::vm::Stop(0))
    }

    pub fn args(&mut self, args: &[String]) -> R<bool> {
        let mut i = 0;
        while i < args.len() {
            let next = args.get(i + 1).cloned();
            match args[i].as_str() {
                "-l" | "-i" | "-a" => match next {
                    Some(f) => {
                        i += 1;
                        match args[i - 1].as_str() {
                            "-l" => match std::fs::read(&f) {
                                Ok(b) => {
                                    let n = b.len().min(self.vm.mem.len());
                                    self.vm.mem[..n].copy_from_slice(&b[..n]);
                                }
                                Err(_) => self.say(&format!("file not found: {f}")),
                            },
                            "-i" => {
                                self.file(&f)?;
                            }
                            _ => self.asm().file(&f)?,
                        }
                    }
                    None => {
                        let a = args[i].clone();
                        self.say(&format!("error: {a} requires a filename"))
                    }
                },
                "-o" => self.show_opcodes(),
                "-p" => self.print_words(),
                "-q" => return Ok(true),
                _ => {}
            }
            i += 1;
        }
        Ok(false)
    }
}

pub fn run(b4: &mut B4) -> i32 {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match b4.args(&args) {
        Ok(true) => return 0,
        Ok(false) => {}
        Err(crate::vm::Stop(code)) => return code,
    }
    loop {
        let mut line = String::new();
        match std::io::stdin().read_line(&mut line) {
            Ok(0) | Err(_) => break,
            Ok(_) => {}
        }
        let line = line.strip_suffix('\n').unwrap_or(&line);
        let line = line.strip_suffix('\r').unwrap_or(line);
        match b4.line(line) {
            Ok(true) => return 0,
            Ok(false) => {}
            Err(crate::vm::Stop(code)) => return code,
        }
    }
    0
}
