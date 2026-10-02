import B4.Basic

/-!
# b4a, the assembly language, in Lean

`B4.Asm.assemble` reads b4a source the way the Pascal assembler (`ub4asm.pas`)
does, and gives the bytes it lays down, with the addresses of its labels:

* `ad`, `sw`, ... are ops; any other bare word is a call of that label (`cl`);
* `1F` (hex digits, possibly `-`) is one byte, `$12345678` four, `'c` a character;
* `:name` defines a label here, `:1F00` moves here, `:R` puts here in register `R`;
* `` `name `` is a label's address, `>name` the same before the label is defined;
* `@R`, `!R`, `+R`, `^R` are the register ops, and `@name`, `!name` fetch and
  store the cell at a label (`li name ri`, `li name wi`);
* `.i .e .t` (if, else, then), `.w .d .o` (while, do, od), `.f .n` (for, next)
  lay down hops, `..` is a zero byte, `"abc"` characters, `."abc"` a counted string;
* `#` starts a comment.

The `b4a!` term (`B4.AsmSyntax`) assembles a block of b4a written inline in a
Lean file, when the file is compiled.
-/

namespace B4.Asm

/-- What a source assembles to: the bytes written, each at its address, and the
labels. -/
structure Program where
  /-- The bytes, in the order they were laid down. -/
  writes : List (Nat × UInt8)
  /-- Each label with its address. -/
  labels : List (String × Nat)
  /-- Where the first byte went. -/
  org : Nat
  deriving Repr, Inhabited

/-- The two-letter ops. -/
def opcode : String → Option UInt8
  | "ad" => some 0x80 | "sb" => some 0x81 | "ml" => some 0x82 | "dv" => some 0x83
  | "md" => some 0x84 | "sh" => some 0x85 | "an" => some 0x86 | "or" => some 0x87
  | "xr" => some 0x88 | "nt" => some 0x89 | "eq" => some 0x8A | "lt" => some 0x8B
  | "du" => some 0x8C | "sw" => some 0x8D | "ov" => some 0x8E | "zp" => some 0x8F
  | "dc" => some 0x90 | "cd" => some 0x91 | "rb" => some 0x92 | "ri" => some 0x93
  | "wb" => some 0x94 | "wi" => some 0x95 | "lb" => some 0x96 | "li" => some 0x97
  | "rs" => some 0x98 | "ls" => some 0x99 | "jm" => some 0x9A | "hp" => some 0x9B
  | "h0" => some 0x9C | "cl" => some 0x9D | "rt" => some 0x9E | "nx" => some 0x9F
  | "fa" => some 0xA0 | "fs" => some 0xA1 | "fm" => some 0xA2 | "fd" => some 0xA3
  | "fl" => some 0xA4 | "fi" => some 0xA5 | "rn" => some 0xA6 | "ct" => some 0xA7
  | "c0" => some 0xC0 | "c1" => some 0xC1 | "c2" => some 0xF6 | "n1" => some 0xF7
  | "c4" => some 0xF8 | "io" => some 0xFD | "db" => some 0xFE | "hl" => some 0xFF
  | _ => none

/-- A register's number, for a one-character name `@` .. `_`. -/
def regNum? (s : String) : Option Nat :=
  match s.toList with
  | [c] => if '@' ≤ c && c ≤ '_' then some (c.toNat - '@'.toNat) else none
  | _ => none

/-- An uppercase hex digit's value. -/
def hexDigit? (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

/-- A hex number, possibly negative. -/
def unhex? (s : String) : Option Int :=
  let go (cs : List Char) : Option Nat :=
    if cs.isEmpty then none
    else cs.foldlM (fun acc c => (hexDigit? c).map (acc * 16 + ·)) 0
  match s.toList with
  | '-' :: cs => (go cs).map fun n => -(n : Int)
  | cs => (go cs).map Int.ofNat

/-- A token of b4a. -/
inductive Tok where
  /-- One byte, from hex. -/
  | hex (s : String)
  /-- Four bytes, `$...`. -/
  | u32 (s : String)
  /-- `'c`. -/
  | chr (c : Char)
  /-- `:name`. -/
  | def_ (s : String)
  /-- `^R`. -/
  | ivk (s : String)
  /-- `` `name ``. -/
  | adr (s : String)
  /-- `@name`. -/
  | get (s : String)
  /-- `!name`. -/
  | put (s : String)
  /-- `>name`. -/
  | fwd (s : String)
  /-- `+R`. -/
  | ink (s : String)
  /-- `"abc"`. -/
  | raw (s : String)
  /-- `."abc"`. -/
  | counted (s : String)
  /-- `.i` and the other macros, by their letter. -/
  | mac (c : Char)
  /-- A bare word: an op, or a call. -/
  | ref (s : String)
  deriving Repr, BEq

/-- The rest of a word, up to white space. -/
def word (cs : List Char) : List Char × List Char := cs.span (· > ' ')

/-- Split b4a source into tokens. -/
partial def tokenize (cs : List Char) (acc : Array Tok := #[]) : Except String (Array Tok) :=
  match cs with
  | [] => .ok acc
  | c :: rest =>
    if c ≤ ' ' then tokenize rest acc
    else match c with
    | '#' => tokenize (rest.dropWhile (· != '\n')) acc
    | '$' =>
      let (h, rest) := rest.span (hexDigit? · |>.isSome)
      tokenize rest (acc.push (.u32 (String.ofList h)))
    | '\'' =>
      match rest with
      | ch :: rest => tokenize rest (acc.push (.chr ch))
      | [] => .error "a ' at the end of the source"
    | '"' =>
      let (s, rest) := rest.span (· != '"')
      match rest with
      | _ :: rest => tokenize rest (acc.push (.raw (String.ofList s)))
      | [] => .error "an unterminated string"
    | '.' =>
      match rest with
      | '.' :: rest => tokenize rest (acc.push (.hex "0"))
      | '"' :: rest =>
        let (s, rest) := rest.span (· != '"')
        match rest with
        | _ :: rest => tokenize rest (acc.push (.counted (String.ofList s)))
        | [] => .error "an unterminated string"
      | m :: rest =>
        if "ietwdofn".contains m then tokenize rest (acc.push (.mac m))
        else .error s!"unknown macro: .{m}"
      | [] => .error "a . at the end of the source"
    | _ =>
      if c == '-' || (hexDigit? c).isSome then
        let (h, rest) := rest.span (hexDigit? · |>.isSome)
        tokenize rest (acc.push (.hex (String.ofList (c :: h))))
      else
        let (w, rest) := word rest
        let s := String.ofList w
        let t := match c with
          | ':' => Tok.def_ s | '^' => .ivk s | '`' => .adr s | '@' => .get s
          | '!' => .put s | '>' => .fwd s | '+' => .ink s
          | _ => .ref (String.ofList (c :: w))
        tokenize rest (acc.push t)

/-- The assembler's state. -/
structure St where
  here : Nat
  writes : Array (Nat × UInt8) := #[]
  labels : List (String × Nat) := []
  /-- References to labels to fill in at the end: where, and which label. -/
  fixups : List (Nat × String) := []
  /-- The compile-time stack of the control macros. -/
  stack : List Nat := []

namespace St

def emit (st : St) (b : UInt8) : St :=
  { st with writes := st.writes.push (st.here, b), here := st.here + 1 }

def emitv (st : St) (v : UInt32) : St :=
  (((st.emit v.toUInt8).emit (v >>> 8).toUInt8).emit (v >>> 16).toUInt8).emit (v >>> 24).toUInt8

/-- Lay down a label's address, now if it is known, else at the end. -/
def emitAddr (st : St) (name : String) : St :=
  match st.labels.lookup name, regNum? name with
  | some a, _ => st.emitv a.toUInt32
  | none, some r => st.emitv (4 * r).toUInt32
  | none, none => { st.emitv 0 with fixups := (st.here, name) :: st.fixups }

/-- Overwrite the byte at `a`. -/
def patch (st : St) (a : Nat) (b : UInt8) : St :=
  { st with writes := st.writes.push (a, b) }

def push (st : St) (a : Nat) : St := { st with stack := a :: st.stack }

def pop (st : St) : Except String (Nat × St) :=
  match st.stack with
  | a :: rest => .ok (a, { st with stack := rest })
  | [] => .error "unbalanced .i .e .t .w .d .o .f .n"

/-- An op with a hop slot to fill later. -/
def hopSlot (st : St) (op : UInt8) : St := (st.emit op).push (st.here + 1) |>.emit 0

/-- Fill the slot on the stack with a hop to here. -/
def hopHere (st : St) : Except String St := do
  let (slot, st) ← st.pop
  let dist := st.here - slot
  if st.here < slot then throw s!"invalid hop at {st.here}"
  if dist > 126 then throw s!"hop too big: {slot} -> {st.here}"
  .ok (st.patch slot (dist + 1).toUInt8)

/-- Lay down a hop back to the address on the stack. -/
def hopBack (st : St) : Except String St := do
  let (dest, st) ← st.pop
  if dest > st.here then throw s!"invalid hop back at {st.here}"
  if st.here - dest > 128 then throw s!"hop too big: {st.here} -> {dest}"
  .ok (st.emit (Int.toNat ((dest : Int) - st.here + 1 + 256) % 256).toUInt8)

end St

/-- Assemble one token. -/
def step (st : St) : Tok → Except String St
  | .hex s =>
    match unhex? s with
    | some v => .ok (st.emit (Int.toNat (v % 256)).toUInt8)
    | none => .error s!"bad number: {s}"
  | .u32 s =>
    match unhex? s with
    | some v => .ok (st.emitv (Int.toNat (v % 0x100000000)).toUInt32)
    | none => .error s!"bad number: ${s}"
  | .chr c => .ok (st.emit c.toNat.toUInt8)
  | .def_ s =>
    match regNum? s, unhex? s with
    | some r, _ => .ok ((List.range 4).foldl (fun st k =>
        st.patch (4 * r + k) ((st.here >>> (8 * k)) % 256).toUInt8) st)
    | none, some a => .ok { st with here := a.toNat }
    | none, none =>
      if (st.labels.lookup s).isSome then .error s!"label defined twice: {s}"
      else .ok { st with labels := (s, st.here) :: st.labels }
  | .ivk s => match regNum? s with
    | some r => .ok (st.emit r.toUInt8)
    | none => .error s!"bad word: ^{s}"
  | .adr s | .fwd s => .ok (st.emitAddr s)
  | .get s => match regNum? s with
    | some r => .ok (st.emit (0x20 + r).toUInt8)
    | none => .ok ((st.emit 0x97).emitAddr s |>.emit 0x93)
  | .put s => match regNum? s with
    | some r => .ok (st.emit (0x40 + r).toUInt8)
    | none => .ok ((st.emit 0x97).emitAddr s |>.emit 0x95)
  | .ink s => match regNum? s with
    | some r => .ok (st.emit (0x60 + r).toUInt8)
    | none => .error s!"no such word: +{s}"
  | .raw s => .ok (s.toList.foldl (fun st c => st.emit c.toNat.toUInt8) st)
  | .counted s => .ok (s.toList.foldl (fun st c => st.emit c.toNat.toUInt8) (st.emit s.length.toUInt8))
  | .mac 'w' => .ok (st.push st.here)
  | .mac 'i' | .mac 'd' => .ok (st.hopSlot 0x9C)
  | .mac 'o' => do
    -- hop back to the `.w`, then fill the `.d`'s hop to here
    let (slot, st) ← st.pop
    let st ← (st.emit 0x9B).hopBack
    (st.push slot).hopHere
  | .mac 'e' => do
    let (slot, st) ← st.pop
    let st := st.hopSlot 0x9B
    let (slot', st) ← st.pop
    ((st.push slot').push slot).hopHere
  | .mac 't' => st.hopHere
  | .mac 'f' => .ok ((st.emit 0x90).push (st.here + 1))
  | .mac 'n' => (st.emit 0x9F).hopBack
  | .mac c => .error s!"unknown macro: .{c}"
  | .ref s => match opcode s with
    | some op => .ok (st.emit op)
    | none => .ok ((st.emit 0x9D).emitAddr s)

/-- **Assemble b4a source**, from address `org`. -/
def assemble (src : String) (org : Nat := 0x100) : Except String Program := do
  let toks ← tokenize src.toList
  let st ← toks.foldlM step { here := org }
  if !st.stack.isEmpty then throw "unbalanced .i .e .t .w .d .o .f .n"
  let st ← st.fixups.foldlM (fun st (a, name) =>
    match st.labels.lookup name with
    | some v => .ok ((List.range 4).foldl (fun st k =>
        st.patch (a + k) ((v >>> (8 * k)) % 256).toUInt8) st)
    | none => .error s!"unknown word: {name}") st
  .ok ⟨st.writes.toList, st.labels.reverse, org⟩

namespace Program

/-- A label's address. -/
def addr (p : Program) (name : String) : Nat := (p.labels.lookup name).getD 0

/-- The bytes from `org` on, as one block (the last write to an address wins). -/
def code (p : Program) : List UInt8 :=
  let top := p.writes.foldl (fun m (a, _) => max m (a + 1)) p.org
  (List.range (top - p.org)).map fun k =>
    (p.writes.foldl (fun b (a, v) => if a = p.org + k then some v else b) none).getD 0

/-- Write the program into a machine's memory. -/
def load (p : Program) (s : State) : State :=
  { s with mem := p.writes.foldl (fun m (a, v) => m.set! a v) s.mem }

end Program

end B4.Asm
