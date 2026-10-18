use crate::vm::{rega, regn, Stop, Value, Vm, OPS, R, RGO, RHP, RLP};
use std::path::{Path, PathBuf};

pub struct Fwd {
    pub key: String,
    pub at: Value,
}

#[derive(Default)]
pub struct Dict {
    pub ents: Vec<(String, Value)>,
    pub fwds: Vec<Fwd>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Tag {
    Wsp,
    Cmt,
    Hex,
    U32,
    Chr,
    Def,
    Ref,
    Ivk,
    Adr,
    Get,
    Put,
    Ink,
    Fwd,
    If,
    Th,
    El,
    Wh,
    Do,
    Od,
    Fr,
    Nx,
    Lp,
    Rs,
    Cs,
    Ob,
    Cb,
}

pub fn is_reg_char(c: char) -> Option<usize> {
    ('@'..='_').contains(&c).then(|| c as usize - '@' as usize)
}

pub fn b4op(code: &str) -> Option<u8> {
    if code == ".." {
        return Some(0);
    }
    if let Some(&(b, _)) = OPS.iter().find(|o| o.1 == code) {
        return Some(b);
    }
    if code == "hl" {
        return Some(0xFF);
    }
    let c: Vec<char> = code.chars().collect();
    if c.len() == 2 {
        if let Some(r) = is_reg_char(c[1]) {
            return match c[0] {
                '^' => Some(r as u8),
                '@' => Some(r as u8 + 0x20),
                '!' => Some(r as u8 + 0x40),
                '+' => Some(r as u8 + 0x60),
                _ => None,
            };
        }
    }
    None
}

pub fn unhex(tok: &str) -> Option<Value> {
    let (neg, digits) = match tok.strip_prefix('-') {
        Some(d) => (true, d),
        None => (false, tok.strip_prefix('$').unwrap_or(tok)),
    };
    if digits.is_empty() || !digits.chars().all(|c| c.is_ascii_hexdigit()) {
        return None;
    }
    let v = u32::from_str_radix(digits, 16).ok()? as Value;
    Some(if neg { v.wrapping_neg() } else { v })
}

pub fn try_hex(tok: &str) -> Option<Value> {
    if !tok.chars().all(|c| c.is_ascii_digit() || c.is_uppercase()) {
        return None;
    }
    unhex(tok)
}

struct Reader {
    lines: Vec<Vec<char>>,
    line: usize,
    col: usize,
    at_end: bool,
    tok_line: usize,
    tok_col: usize,
}

impl Reader {
    fn next_char(&mut self) -> char {
        let mut ch = '\0';
        if self.line >= self.lines.len() {
            self.at_end = true;
        } else if self.col > self.lines[self.line].len() {
            self.col = 1;
            self.line += 1;
            if self.line >= self.lines.len() {
                self.at_end = true;
            } else {
                ch = '\n';
            }
        } else {
            ch = self.lines[self.line][self.col - 1];
            self.col += 1;
        }
        if self.at_end {
            ch = '\0';
        }
        ch
    }
}

impl Dict {
    pub fn find(&self, s: &str) -> Option<Value> {
        self.ents.iter().rev().find(|e| e.0 == s).map(|e| e.1)
    }

    pub fn clear(&mut self) {
        self.ents.clear();
        self.fwds.clear();
    }
}

pub struct Asm<'a> {
    pub vm: &'a mut Vm,
    pub dict: &'a mut Dict,
}

impl Asm<'_> {
    fn die<T>(&mut self, r: &Reader, msg: &str) -> R<T> {
        self.vm.say(&format!("{}:{} {msg}", r.tok_line + 1, r.tok_col));
        Err(Stop(1))
    }

    fn next(&mut self, r: &mut Reader, ch: &mut char) -> R<Option<(Tag, String)>> {
        let mut s = ch.to_string();
        r.tok_line = r.line;
        r.tok_col = r.col;
        macro_rules! rest {
            ($t:expr) => {{
                s.clear();
                while !r.at_end {
                    *ch = r.next_char();
                    if *ch > ' ' {
                        s.push(*ch)
                    } else {
                        break;
                    }
                }
                $t
            }};
        }
        macro_rules! rdhex {
            () => {{
                while !r.at_end {
                    *ch = r.next_char();
                    if ch.is_ascii_digit() || ('A'..='F').contains(ch) {
                        s.push(*ch)
                    } else {
                        break;
                    }
                }
            }};
        }
        macro_rules! rdstr {
            () => {{
                s.clear();
                loop {
                    if r.at_end {
                        break;
                    }
                    *ch = r.next_char();
                    if *ch == '"' {
                        break;
                    }
                    s.push(*ch);
                }
                if r.at_end {
                    return self.die(r, "unterminated string literal");
                }
                *ch = r.next_char();
            }};
        }
        let tag = match *ch {
            '\0' => return Ok(None),
            '\u{1}'..=' ' => {
                loop {
                    if r.at_end {
                        break;
                    }
                    *ch = r.next_char();
                    if *ch >= ' ' {
                        break;
                    }
                }
                Tag::Wsp
            }
            '#' => {
                while !r.at_end && *ch != '\n' {
                    *ch = r.next_char();
                }
                Tag::Cmt
            }
            '$' => {
                rdhex!();
                Tag::U32
            }
            '-' | '0'..='9' | 'A'..='F' => {
                rdhex!();
                Tag::Hex
            }
            '\'' => {
                s = r.next_char().to_string();
                *ch = r.next_char();
                Tag::Chr
            }
            ':' => rest!(Tag::Def),
            '^' => rest!(Tag::Ivk),
            '`' => rest!(Tag::Adr),
            '@' => rest!(Tag::Get),
            '!' => rest!(Tag::Put),
            '>' => rest!(Tag::Fwd),
            '+' => rest!(Tag::Ink),
            '"' => {
                rdstr!();
                Tag::Rs
            }
            '.' => {
                *ch = r.next_char();
                let t = match *ch {
                    '.' => {
                        s = "0".into();
                        *ch = r.next_char();
                        Tag::Hex
                    }
                    '^' => Tag::Lp,
                    'i' => Tag::If,
                    'e' => Tag::El,
                    't' => Tag::Th,
                    'w' => Tag::Wh,
                    'd' => Tag::Do,
                    'o' => Tag::Od,
                    'f' => Tag::Fr,
                    'n' => Tag::Nx,
                    '"' => {
                        rdstr!();
                        Tag::Cs
                    }
                    '[' => Tag::Ob,
                    ']' => Tag::Cb,
                    c => return self.die(r, &format!("unknown macro: .{c}")),
                };
                *ch = r.next_char();
                t
            }
            _ => {
                while !r.at_end {
                    *ch = r.next_char();
                    if *ch > ' ' {
                        s.push(*ch)
                    } else {
                        break;
                    }
                }
                Tag::Ref
            }
        };
        Ok(Some((tag, s)))
    }

    fn emit(&mut self, here: &mut Value, v: Value) -> R<()> {
        self.vm.bset(*here, v as u8)?;
        *here += 1;
        Ok(())
    }

    fn emitv(&mut self, here: &mut Value, v: Value) -> R<()> {
        self.vm.wrval(*here, v)?;
        *here += 4;
        Ok(())
    }

    fn op(&mut self, r: &Reader, code: &str) -> R<Value> {
        match self.vm.op_byte(code) {
            Some(b) => Ok(b as Value),
            None => self.die(r, &format!("invalid op: {code}")),
        }
    }

    fn find_addr(&mut self, r: &Reader, s: &str) -> R<Value> {
        if let Some(a) = self.dict.find(s) {
            return Ok(a);
        }
        let c: Vec<char> = s.chars().collect();
        if c.len() == 1 && is_reg_char(c[0]).is_some() {
            return Ok(rega(c[0]));
        }
        self.die(r, &format!("unknown word:{s}"))
    }

    fn compile(&mut self, r: &Reader, here: &mut Value, tag: Tag, s: &str) -> R<()> {
        let isreg = |s: &str| {
            let c: Vec<char> = s.chars().collect();
            (c.len() == 1 && is_reg_char(c[0]).is_some()).then(|| c[0])
        };
        match tag {
            Tag::Wsp | Tag::Cmt => {}
            Tag::Hex | Tag::U32 => {
                let Some(v) = unhex(s) else { return self.die(r, &format!("bad hex: {s}")) };
                if tag == Tag::Hex {
                    self.emit(here, v)?
                } else {
                    self.emitv(here, v)?
                }
            }
            Tag::Chr => {
                if s.chars().count() > 1 {
                    return self.die(r, &format!("bad char: {s}"));
                }
                let c = s.chars().next().unwrap_or('\0');
                self.emit(here, c as Value)?
            }
            Tag::Def => {
                if let Some(c) = isreg(s) {
                    self.vm.set_rg(regn(c), *here);
                } else if let Some(a) = s.strip_prefix('+').and_then(try_hex).filter(|_| s.len() > 1) {
                    *here += a;
                } else if let Some(a) = try_hex(s) {
                    *here = a;
                } else if self.dict.ents.len() >= 4096 {
                    return self.die(r, "too many :defs");
                } else {
                    self.dict.ents.push((s.to_owned(), *here));
                    if s == "main" {
                        self.vm.set_rg(RGO, *here);
                    }
                    let mut i = 0;
                    while i < self.dict.fwds.len() {
                        if self.dict.fwds[i].key == s {
                            let at = self.dict.fwds[i].at;
                            self.vm.wrval(at, *here)?;
                            self.dict.fwds.remove(i);
                        } else {
                            i += 1;
                        }
                    }
                }
            }
            Tag::Ref => match self.vm.op_byte(s) {
                Some(b) => self.emit(here, b as Value)?,
                None => {
                    let a = self.find_addr(r, s)?;
                    let cl = self.op(r, "cl")?;
                    self.emit(here, cl)?;
                    self.emitv(here, a)?
                }
            },
            Tag::Ivk => match isreg(s) {
                Some(c) => {
                    let b = self.op(r, &format!("^{c}"))?;
                    self.emit(here, b)?
                }
                None => return self.die(r, &format!("bad word: ^{s}")),
            },
            Tag::Fwd => {
                self.dict.fwds.push(Fwd { key: s.to_owned(), at: *here });
                self.emitv(here, 0)?
            }
            Tag::Adr => {
                let a = self.find_addr(r, s)?;
                self.emitv(here, a)?
            }
            Tag::Get | Tag::Put => {
                let p = if tag == Tag::Get { "@" } else { "!" };
                match isreg(s) {
                    Some(c) => {
                        let b = self.op(r, &format!("{p}{c}"))?;
                        self.emit(here, b)?
                    }
                    None => {
                        let li = self.op(r, "li")?;
                        self.emit(here, li)?;
                        let a = self.find_addr(r, s)?;
                        self.emitv(here, a)?;
                        let o = self.op(r, if tag == Tag::Get { "ri" } else { "wi" })?;
                        self.emit(here, o)?
                    }
                }
            }
            Tag::Ink => match isreg(s) {
                Some(c) => {
                    let b = self.op(r, &format!("+{c}"))?;
                    self.emit(here, b)?
                }
                None => return self.die(r, &format!("no such word: +{s}")),
            },
            Tag::Wh => self.vm.dput(*here)?,
            Tag::If | Tag::Do => self.hop_slot(r, here, "h0")?,
            Tag::Od => {
                let hp = self.op(r, "hp")?;
                self.emit(here, hp)?;
                self.vm.dswp()?;
                self.hop_back(r, here)?;
                self.hop_here(r, here)?
            }
            Tag::El => {
                self.hop_slot(r, here, "hp")?;
                self.vm.dswp()?;
                self.hop_here(r, here)?
            }
            Tag::Th => self.hop_here(r, here)?,
            Tag::Fr => {
                let dc = self.op(r, "dc")?;
                self.emit(here, dc)?;
                self.vm.dput(*here)?
            }
            Tag::Nx => {
                let nx = self.op(r, "nx")?;
                self.emit(here, nx)?;
                self.hop_back(r, here)?
            }
            Tag::Lp => {
                let lp = self.vm.rg(RLP);
                self.emitv(here, lp)?;
                self.vm.set_rg(RLP, *here - 4)
            }
            Tag::Rs => {
                for b in s.bytes() {
                    self.emit(here, b as Value)?
                }
            }
            Tag::Cs => {
                self.emit(here, s.len() as Value)?;
                for b in s.bytes() {
                    self.emit(here, b as Value)?
                }
            }
            Tag::Ob => self.hop_slot(r, here, "hp")?,
            Tag::Cb => {
                let t = self.vm.tos()?;
                self.vm.dput(t)?;
                self.hop_here(r, here)?;
                let li = self.op(r, "li")?;
                self.emit(here, li)?;
                let a = self.vm.dpop()?;
                self.emitv(here, a + 1)?
            }
        }
        Ok(())
    }

    fn hop_slot(&mut self, r: &Reader, here: &mut Value, op: &str) -> R<()> {
        let b = self.op(r, op)?;
        self.emit(here, b)?;
        self.vm.dput(*here)?;
        self.emit(here, 0)
    }

    fn hop_here(&mut self, r: &Reader, here: &mut Value) -> R<()> {
        let slot = self.vm.dpop()?;
        let dist = *here - slot;
        if dist < 0 {
            return self.die(r, &format!("invalid hop_here at {here}"));
        }
        if dist > 126 {
            return self.die(r, &format!("hop too big: {here} -> {slot}"));
        }
        self.vm.bset(slot, (dist + 1) as u8)
    }

    fn hop_back(&mut self, r: &Reader, here: &mut Value) -> R<()> {
        let dest = self.vm.dpop()?;
        let dist = dest - *here;
        if dist > 0 {
            return self.die(r, &format!("invalid hop_back at {here}"));
        }
        if dist < -128 {
            return self.die(r, &format!("hop too big: {here} -> {dest}"));
        }
        self.emit(here, (dist + 1) as u8 as Value)
    }

    pub fn strs(&mut self, lines: &[String]) -> R<()> {
        let mut r = Reader { lines: lines.iter().map(|l| l.chars().collect()).collect(), line: 0, col: 1, at_end: lines.is_empty(), tok_line: 0, tok_col: 0 };
        let mut here = self.vm.rg(RHP);
        let mut ch = r.next_char();
        while !r.at_end {
            if let Some((tag, s)) = self.next(&mut r, &mut ch)? {
                self.compile(&r, &mut here, tag, &s)?;
            }
        }
        self.vm.set_rg(RHP, here);
        Ok(())
    }

    pub fn line(&mut self, s: &str) -> R<()> {
        let lines: Vec<String> = s.lines().map(str::to_owned).collect();
        self.strs(&lines)
    }

    pub fn file(&mut self, path: &str) -> R<()> {
        let mut lines = vec![];
        self.expand(Path::new(path), &mut lines)?;
        self.strs(&lines)
    }

    fn expand(&mut self, path: &Path, out: &mut Vec<String>) -> R<()> {
        let Ok(text) = std::fs::read_to_string(path) else {
            self.vm.say(&format!("file not found: {}", path.display()));
            return Ok(());
        };
        let base = std::path::absolute(path).ok().and_then(|p| p.parent().map(Path::to_path_buf)).unwrap_or_default();
        for line in text.lines() {
            let t = line.trim_start();
            let directive = (t.starts_with("/a") || t.starts_with("/i")) && (t.len() == 2 || t.as_bytes()[2] <= b' ');
            if directive {
                let arg = t[2..].trim();
                if arg.is_empty() {
                    self.vm.say(&format!("0:0 include directive requires a filename: {line}"));
                    return Err(Stop(1));
                }
                let inc = if Path::new(arg).parent().is_none_or(|p| p.as_os_str().is_empty()) { base.join(arg) } else { PathBuf::from(arg) };
                self.expand(&inc, out)?;
            } else {
                out.push(line.to_owned());
            }
        }
        Ok(())
    }
}
