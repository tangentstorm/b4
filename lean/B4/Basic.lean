import Std

namespace B4

def MAXBYTE : Nat := 65536
def STACKSZ : Nat := 256

-- Register offsets
def RIP_OFF : Nat := 132
def RDS_OFF : Nat := 140
def RCS_OFF : Nat := 152
def RST_OFF : Nat := 160
def RDB_OFF : Nat := 164

inductive Register where
  | PC | DS | CS | ST | DB | RED | BLU | GRN | HERE | T | X | Y | Z
  | R (n : Nat)
deriving BEq, Inhabited

def Register.toNat : Register → Nat
  | PC => 33
  | DS => 35
  | CS => 38
  | ST => 40
  | DB => 41
  | RED => 42
  | BLU => 43
  | GRN => 44
  | HERE => 31
  | T => 20
  | X => 24
  | Y => 25
  | Z => 26
  | R n => n % 32

inductive Op where
  | nop
  | ad | sb | ml | dv | md | sh | an | or | xr | nt | eq | lt
  | du | sw | ov | zp
  | dc | cd
  | rb | ri | wb | wi
  | lb (v : UInt8)
  | li (v : UInt32)
  | rs | ls
  | jm (addr : UInt32)
  | hp (dist : Int8)
  | h0 (dist : Int8)
  | cl (addr : UInt32)
  | rt
  | nx (dist : Int8)
  | c0 | c1 | c2 | n1 | c4
  | io | db | hl
  | invoke (r : Register)
  | read   (r : Register)
  | write  (r : Register)
  | stream (r : Register)
deriving BEq, Inhabited

def Op.toByte : Op → UInt8
  | nop => 0
  | ad => 0x80 | sb => 0x81 | ml => 0x82 | dv => 0x83
  | md => 0x84 | sh => 0x85 | an => 0x86 | or => 0x87
  | xr => 0x88 | nt => 0x89 | eq => 0x8A | lt => 0x8B
  | du => 0x8C | sw => 0x8D | ov => 0x8E | zp => 0x8F
  | dc => 0x90 | cd => 0x91 | rb => 0x92 | ri => 0x93
  | wb => 0x94 | wi => 0x95 | lb _ => 0x96 | li _ => 0x97
  | rs => 0x98 | ls => 0x99 | jm _ => 0x9A | hp _ => 0x9B
  | h0 _ => 0x9C | cl _ => 0x9D | rt => 0x9E | nx _ => 0x9F
  | c0 => 0xC0 | c1 => 0xC1 | c2 => 0xF6 | n1 => 0xF7 | c4 => 0xF8
  | io => 0xFD | db => 0xFE | hl => 0xFF
  | invoke r => r.toNat.toUInt8
  | read r   => 0x20 + r.toNat.toUInt8
  | write r  => 0x40 + r.toNat.toUInt8
  | stream r => 0x60 + r.toNat.toUInt8

structure State where
  mem : ByteArray
  ds  : Array UInt32
  cs  : Array UInt32
  ob  : String := ""
deriving Inhabited

def mkInitialState : State :=
  { mem := ByteArray.mk (Array.replicate MAXBYTE 0),
    ds  := Array.replicate STACKSZ 0,
    cs  := Array.replicate STACKSZ 0,
    ob  := "" }

-- Helper to read 32-bit LE
def getVal (mem : ByteArray) (off : Nat) : UInt32 :=
  if off + 3 < mem.size then
    let b0 := (mem.get! off).toUInt32
    let b1 := (mem.get! (off + 1)).toUInt32
    let b2 := (mem.get! (off + 2)).toUInt32
    let b3 := (mem.get! (off + 3)).toUInt32
    b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)
  else 0

-- Helper to write 32-bit LE
def setVal (mem : ByteArray) (off : Nat) (v : UInt32) : ByteArray :=
  if off + 3 < mem.size then
    let mem := mem.set! off (v.toUInt8)
    let mem := mem.set! (off + 1) ((v >>> 8).toUInt8)
    let mem := mem.set! (off + 2) ((v >>> 16).toUInt8)
    let mem := mem.set! (off + 3) ((v >>> 24).toUInt8)
    mem
  else mem

-- Accessors
def getIP (s : State) : Nat := (getVal s.mem RIP_OFF).toNat
def setIP (s : State) (v : Nat) : State := { s with mem := setVal s.mem RIP_OFF v.toUInt32 }

def getDSH (s : State) : Nat := (getVal s.mem RDS_OFF).toNat
def setDSH (s : State) (v : Nat) : State := { s with mem := setVal s.mem RDS_OFF v.toUInt32 }

def getCSH (s : State) : Nat := (getVal s.mem RCS_OFF).toNat
def setCSH (s : State) (v : Nat) : State := { s with mem := setVal s.mem RCS_OFF v.toUInt32 }

def getRST (s : State) : UInt32 := getVal s.mem RST_OFF
def setRST (s : State) (v : UInt32) : State := { s with mem := setVal s.mem RST_OFF v }

def getRDB (s : State) : UInt32 := getVal s.mem RDB_OFF
def setRDB (s : State) (v : UInt32) : State := { s with mem := setVal s.mem RDB_OFF v }

-- Stack operations
def dpush (s : State) (v : UInt32) : State :=
  let h := getDSH s
  if h < STACKSZ then
    let s := { s with ds := s.ds.set! h v }
    setDSH s (h + 1)
  else s

def dpop (s : State) : (UInt32 × State) :=
  let h := getDSH s
  if h > 0 then
    let v := s.ds[h - 1]!
    let s := setDSH s (h - 1)
    (v, s)
  else (0, s)

def cpush (s : State) (v : UInt32) : State :=
  let h := getCSH s
  if h < STACKSZ then
    let s := { s with cs := s.cs.set! h v }
    setCSH s (h + 1)
  else s

def cpop (s : State) : (UInt32 × State) :=
  let h := getCSH s
  if h > 0 then
    let v := s.cs[h - 1]!
    let s := setCSH s (h - 1)
    (v, s)
  else (0, s)

-- Signed operations
def toInt32 (v : UInt32) : Int :=
  let n := v.toNat
  if n >= 0x80000000 then (n : Int) - 0x100000000 else (n : Int)

def fromInt32 (i : Int) : UInt32 :=
  let n := if i < 0 then i + 0x100000000 else i
  UInt32.ofNat (n.toNat % 0x100000000)

-- Relative jumps
def go (s : State) (addr : Nat) : State :=
  let v := if addr < 0x100 then 0x100 else addr
  setIP s (v - 1)

def hop (s : State) : State :=
  let ip := getIP s
  let dist := s.mem.get! (ip + 1)
  let d : Int := if dist >= 128 then (dist.toNat : Int) - 256 else (dist.toNat : Int)
  go s (Int.ofNat ip + d).toNat

-- Opcode implementation
def runOp (s : State) (op : UInt8) : State :=
  if op == 0 then s
  else if op < 0x20 then -- ^R Invoke
    let r := 4 * op.toNat
    let s := cpush s (getIP s + 1).toUInt32
    go s (getVal s.mem r).toNat
  else if op < 0x40 then -- @R Read
    let r := 4 * (op.toNat % 32)
    dpush s (getVal s.mem r)
  else if op < 0x60 then -- !R Write
    let r := 4 * (op.toNat % 32)
    let (v, s) := dpop s
    { s with mem := setVal s.mem r v }
  else if op < 0x80 then -- +R Stream
    let r := 4 * (op.toNat % 32)
    let (d, s) := dpop s
    let v := getVal s.mem r
    let s := dpush s v
    { s with mem := setVal s.mem r (fromInt32 (toInt32 v + toInt32 d)) }
  else match op with
    | 0x80 => -- ad
        let (y, s) := dpop s
        let (x, s) := dpop s
        dpush s (fromInt32 (toInt32 x + toInt32 y))
    | 0x81 => -- sb
        let (y, s) := dpop s
        let (x, s) := dpop s
        dpush s (fromInt32 (toInt32 x - toInt32 y))
    | 0x82 => -- ml
        let (y, s) := dpop s
        let (x, s) := dpop s
        dpush s (fromInt32 (toInt32 x * toInt32 y))
    | 0x83 => -- dv
        let (y, s) := dpop s
        let (x, s) := dpop s
        if y == 0 then s else dpush s (fromInt32 (toInt32 x / toInt32 y))
    | 0x84 => -- md
        let (y, s) := dpop s
        let (x, s) := dpop s
        if y == 0 then s else dpush s (fromInt32 (toInt32 x % toInt32 y))
    | 0x85 => -- sh
        let (t, s) := dpop s
        let (x, s) := dpop s
        let shift := toInt32 t
        if shift < 0 then
          dpush s (UInt32.shiftRight x (-shift).toNat.toUInt32)
        else
          dpush s (UInt32.shiftLeft x shift.toNat.toUInt32)
    | 0x86 => -- an
        let (y, s) := dpop s
        let (x, s) := dpop s
        dpush s (x &&& y)
    | 0x87 => -- or
        let (y, s) := dpop s
        let (x, s) := dpop s
        dpush s (x ||| y)
    | 0x88 => -- xr
        let (y, s) := dpop s
        let (x, s) := dpop s
        dpush s (x ^^^ y)
    | 0x89 => -- nt
        let (x, s) := dpop s
        dpush s (x ^^^ 0xFFFFFFFF)
    | 0x8A => -- eq
        let (y, s) := dpop s
        let (x, s) := dpop s
        dpush s (if x == y then 0xFFFFFFFF else 0)
    | 0x8B => -- lt
        let (y, s) := dpop s
        let (x, s) := dpop s
        dpush s (if toInt32 x < toInt32 y then 0xFFFFFFFF else 0)
    | 0x8C => -- du
        let h := getDSH s
        if h > 0 then dpush s (s.ds[h - 1]!) else s
    | 0x8D => -- sw
        let (y, s) := dpop s
        let (x, s) := dpop s
        let s := dpush s y
        dpush s x
    | 0x8E => -- ov
        let h := getDSH s
        if h > 1 then dpush s (s.ds[h - 2]!) else s
    | 0x8F => -- zp
        let (_, s) := dpop s
        s
    | 0x90 => -- dc
        let (v, s) := dpop s
        cpush s v
    | 0x91 => -- cd
        let (v, s) := cpop s
        dpush s v
    | 0x92 => -- rb
        let (a, s) := dpop s
        dpush s (s.mem.get! a.toNat).toUInt32
    | 0x93 => -- ri
        let (a, s) := dpop s
        dpush s (getVal s.mem a.toNat)
    | 0x94 => -- wb
        let (a, s) := dpop s
        let (v, s) := dpop s
        { s with mem := s.mem.set! a.toNat v.toUInt8 }
    | 0x95 => -- wi
        let (a, s) := dpop s
        let (v, s) := dpop s
        { s with mem := setVal s.mem a.toNat v }
    | 0x96 => -- lb
        let ip := getIP s
        let v := s.mem.get! (ip + 1)
        let s := dpush s v.toUInt32
        setIP s (ip + 1)
    | 0x97 => -- li
        let ip := getIP s
        let v := getVal s.mem (ip + 1)
        let s := dpush s v
        setIP s (ip + 4)
    | 0x98 => -- rs
        let (a, s) := dpop s
        let v := s.mem.get! a.toNat
        let sv : Int := if v >= 128 then (v.toNat : Int) - 256 else (v.toNat : Int)
        dpush s (fromInt32 sv)
    | 0x99 => -- ls
        let ip := getIP s
        let v := s.mem.get! (ip + 1)
        let sv : Int := if v >= 128 then (v.toNat : Int) - 256 else (v.toNat : Int)
        let s := dpush s (fromInt32 sv)
        setIP s (ip + 1)
    | 0x9A => -- jm
        let ip := getIP s
        go s (getVal s.mem (ip + 1)).toNat
    | 0x9B => -- hp
        hop s
    | 0x9C => -- h0
        let (v, s) := dpop s
        if v == 0 then hop s else setIP s (getIP s + 1)
    | 0x9D => -- cl
        let ip := getIP s
        let s := cpush s (ip + 5).toUInt32
        go s (getVal s.mem (ip + 1)).toNat
    | 0x9E => -- rt
        let (a, s) := cpop s
        if a == 0 then setRST s 0 else setIP s (a.toNat - 1)
    | 0x9F => -- nx
        let h := getCSH s
        if h > 0 then
          let v := s.cs[h - 1]!
          if v > 0 then
            let v' := v - 1
            if v' > 0 then
              let s := { s with cs := s.cs.set! (h - 1) v' }
              hop s
            else
              let (_, s) := cpop s
              setIP s (getIP s + 1)
          else
            let (_, s) := cpop s
            setIP s (getIP s + 1)
        else s
    | 0xC0 => dpush s 0
    | 0xC1 => dpush s 1
    | 0xF6 => dpush s 2
    | 0xF7 => dpush s 0xFFFFFFFF
    | 0xF8 => dpush s 4
    | 0xFD => -- io
        let (cmd, s) := dpop s
        match Char.ofNat cmd.toNat with
        | 'e' =>
            let (ch, s) := dpop s
            let c := if ch < 32 then ' ' else Char.ofNat ch.toNat
            { s with ob := s.ob.push c }
        | 'o' =>
            let (ch, s) := dpop s
            { s with ob := s.ob.push (Char.ofNat ch.toNat) }
        | _ => s
    | 0xFE => setRDB s 1
    | 0xFF => setRST s 0
    | _ => s

def step (s : State) : State :=
  let ip := getIP s
  let op := s.mem.get! ip
  let s := runOp s op
  setIP s (getIP s + 1)

partial def run (s : State) : State :=
  if getRST s == 1 && getRDB s == 0 then
    run (step s)
  else
    s

def assemble (ops : List Op) : ByteArray :=
  let rec loop (acc : ByteArray) (ops : List Op) : ByteArray :=
    match ops with
    | [] => acc
    | op :: os =>
      let acc := acc.push op.toByte
      let acc := match op with
        | Op.lb v => acc.push v
        | Op.li v => 
            let acc := acc.push v.toUInt8
            let acc := acc.push (v >>> 8).toUInt8
            let acc := acc.push (v >>> 16).toUInt8
            acc.push (v >>> 24).toUInt8
        | Op.jm v | Op.cl v =>
            let acc := acc.push v.toUInt8
            let acc := acc.push (v >>> 8).toUInt8
            let acc := acc.push (v >>> 16).toUInt8
            acc.push (v >>> 24).toUInt8
        | Op.hp v | Op.h0 v | Op.nx v =>
            acc.push v.toUInt8
        | _ => acc
      loop acc os
  loop ByteArray.empty ops

end B4
