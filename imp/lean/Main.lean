import B4
import Std

def toB4Mat (v : UInt32) : String :=
  let i := B4.toInt32 v
  if i < 0 then
    "-" ++ (Nat.toDigits 16 (-i).toNat |> String.ofList |>.map Char.toUpper)
  else
    Nat.toDigits 16 i.toNat |> String.ofList |>.map Char.toUpper

def toNat16 (s : String) : Option Nat :=
  let chars := s.toList
  let rec loop (acc : Nat) (chars : List Char) : Option Nat :=
    match chars with
    | [] => some acc
    | c :: cs =>
      let val := if c.isDigit then c.toNat - '0'.toNat
                 else if c >= 'A' && c <= 'F' then c.toNat - 'A'.toNat + 10
                 else if c >= 'a' && c <= 'f' then c.toNat - 'a'.toNat + 10
                 else 255
      if val < 16 then loop (acc * 16 + val) cs else none
  if chars.isEmpty then none else loop 0 chars

def toInt16 (s : String) : Option Int :=
  if s.startsWith "-" then
    match toNat16 (s.toList.drop 1 |> String.ofList) with
    | some n => some (- (n : Int))
    | none => none
  else
    match toNat16 s with
    | some n => some (n : Int)
    | none => none

def regNum (c : Char) : Nat :=
  let c := c.toUpper
  if c >= '@' && c <= '_' then c.toNat - '@'.toNat else 255

def padLeft (s : String) (n : Nat) (c : Char) : String :=
  let len := s.length
  if len < n then
    let pad := String.ofList (List.replicate (n - len) c)
    pad ++ s
  else s

def mnemonics : Array String :=
  let m := Array.replicate 256 ""
  let m := m.set! 0x80 "ad" |>.set! 0x81 "sb" |>.set! 0x82 "ml" |>.set! 0x83 "dv"
  let m := m.set! 0x84 "md" |>.set! 0x85 "sh" |>.set! 0x86 "an" |>.set! 0x87 "or"
  let m := m.set! 0x88 "xr" |>.set! 0x89 "nt" |>.set! 0x8A "eq" |>.set! 0x8B "lt"
  let m := m.set! 0x8C "du" |>.set! 0x8D "sw" |>.set! 0x8E "ov" |>.set! 0x8F "zp"
  let m := m.set! 0x90 "dc" |>.set! 0x91 "cd" |>.set! 0x92 "rb" |>.set! 0x93 "ri"
  let m := m.set! 0x94 "wb" |>.set! 0x95 "wi" |>.set! 0x96 "lb" |>.set! 0x97 "li"
  let m := m.set! 0x98 "rs" |>.set! 0x99 "ls" |>.set! 0x9A "jm" |>.set! 0x9B "hp"
  let m := m.set! 0x9C "h0" |>.set! 0x9D "cl" |>.set! 0x9E "rt" |>.set! 0x9F "nx"
  let m := m.set! 0xBE "tm" |>.set! 0xC0 "c0" |>.set! 0xC1 "c1" |>.set! 0xF6 "c2"
  let m := m.set! 0xF7 "n1" |>.set! 0xF8 "c4" |>.set! 0xFD "io" |>.set! 0xFE "db"
  let m := m.set! 0xFF "hl"
  m

def toMnemonic (v : UInt8) : String :=
  if v == 0 then ".."
  else if v < 0x20 then "^" ++ (String.ofList [Char.ofNat (0x40 + v.toNat)])
  else if v < 0x40 then "@" ++ (String.ofList [Char.ofNat (0x40 + v.toNat - 0x20)])
  else if v < 0x60 then "!" ++ (String.ofList [Char.ofNat (0x40 + v.toNat - 0x40)])
  else if v < 0x80 then "+" ++ (String.ofList [Char.ofNat (0x40 + v.toNat - 0x60)])
  else
    let m := mnemonics[v.toNat]!
    if m != "" then m else (Nat.toDigits 16 v.toNat |> String.ofList |>.map Char.toUpper)

def getOp (s : String) : Option UInt8 :=
  match s with
  | "ad" => some 0x80 | "sb" => some 0x81 | "ml" => some 0x82 | "dv" => some 0x83
  | "md" => some 0x84 | "sh" => some 0x85 | "an" => some 0x86 | "or" => some 0x87
  | "xr" => some 0x88 | "nt" => some 0x89 | "eq" => some 0x8A | "lt" => some 0x8B
  | "du" => some 0x8C | "sw" => some 0x8D | "ov" => some 0x8E | "zp" => some 0x8F
  | "dc" => some 0x90 | "cd" => some 0x91 | "rb" => some 0x92 | "ri" => some 0x93
  | "wb" => some 0x94 | "wi" => some 0x95 | "lb" => some 0x96 | "li" => some 0x97
  | "rs" => some 0x98 | "ls" => some 0x99 | "jm" => some 0x9A | "hp" => some 0x9B
  | "h0" => some 0x9C | "cl" => some 0x9D | "rt" => some 0x9E | "nx" => some 0x9F
  | "c0" => some 0xC0 | "c1" => some 0xC1 | "c2" => some 0xF6 | "n1" => some 0xF7
  | "c4" => some 0xF8 | "io" => some 0xFD | "db" => some 0xFE | "hl" => some 0xFF
  | ".." => some 0x00
  | _ =>
    let cs := s.toList
    if cs.length == 2 then
      let c := cs[0]!
      let r := regNum cs[1]!
      if r < 32 then
        match c with
        | '^' => some (r.toUInt8)
        | '@' => some (0x20 + r.toUInt8)
        | '!' => some (0x40 + r.toUInt8)
        | '+' => some (0x60 + r.toUInt8)
        | _ => none
      else none
    else none

structure State where
  vm : B4.State
  labels : List (String × Nat) := []
deriving Inhabited

def findLabel (labels : List (String × Nat)) (name : String) : Option Nat :=
  match labels with
  | [] => none
  | (n, a) :: ls => if n == name then some a else findLabel ls name

partial def loop (s : State) : IO Unit := do
  let stdin ← IO.getStdin
  let line ← stdin.getLine
  if line.isEmpty then return
  
  let mut s' := s
  let chars := line.toList
  let mut i := 0
  while i < chars.length do
    while i < chars.length && chars[i]! <= ' ' do i := i + 1
    if i >= chars.length then break
    
    let char := chars[i]!
    if char == '\'' then
      if i + 1 < chars.length then
        let c := chars[i+1]!
        s' := { s' with vm := B4.dpush s'.vm c.toNat.toUInt32 }
        i := i + 2
      else i := i + 1
    else if char == '`' then
      if i + 1 < chars.length then
        let r := regNum chars[i+1]!
        if r < 32 then s' := { s' with vm := B4.dpush s'.vm (r * 4).toUInt32 }
        i := i + 2
      else i := i + 1
    else
      let mut token_chars := []
      while i < chars.length && chars[i]! > ' ' do
        token_chars := token_chars ++ [chars[i]!]
        i := i + 1
      let token := String.ofList token_chars
      
      if token == "/q" then return
      else if token == "/s" then
        s' := { s' with vm := B4.step s'.vm }
      else if token == "/g" then
        s' := { s' with vm := B4.setRST s'.vm 1 }
        s' := { s' with vm := B4.setRDB s'.vm 0 }
        s' := { s' with vm := B4.run s'.vm }
        s' := { s' with vm := B4.setRST s'.vm 1 }
      else if token == "/C" then
        s' := { vm := B4.mkInitialState, labels := [] }
        s' := { s' with vm := B4.setRST s'.vm 1 }
        s' := { s' with vm := B4.setIP s'.vm 0x100 }
        s' := { s' with vm := B4.setVal s'.vm.mem 124 0x100 |> fun m => { s'.vm with mem := m } }
      else if token == "?d" then
        let h := B4.getDSH s'.vm
        let mut ds_str := "ds: ["
        for j in [0:h] do
          if j > 0 then ds_str := ds_str ++ " "
          ds_str := ds_str ++ toB4Mat s'.vm.ds[j]!
        ds_str := ds_str ++ "]"
        IO.println ds_str
      else if token == "?c" then
        let h := B4.getCSH s'.vm
        let mut cs_str := "cs: ["
        for j in [0:h] do
          if j > 0 then cs_str := cs_str ++ " "
          cs_str := cs_str ++ toB4Mat s'.vm.cs[j]!
        cs_str := cs_str ++ "]"
        IO.println cs_str
      else if token == "?i" then
        IO.println s!"ip: {toB4Mat (B4.getVal s'.vm.mem 132)}"
      else if token.startsWith ":" then
        let rest_list := token.toList.drop 1
        let rest := String.ofList rest_list
        let mut here := (B4.getVal s'.vm.mem 124).toNat
        if rest.isEmpty then pure ()
        else if rest_list.length == 1 && regNum rest_list[0]! < 32 then
          let r := regNum rest_list[0]!
          s' := { s' with vm := { s'.vm with mem := B4.setVal s'.vm.mem (r * 4) here.toUInt32 } }
        else if let some addr := toNat16 rest then
          here := addr
        else
          s' := { s' with labels := (rest, here) :: s'.labels }
        
        while i < chars.length do
          while i < chars.length && chars[i]! <= ' ' do i := i + 1
          if i >= chars.length then break
          let t_start := i
          if chars[i]! == '\'' then
            if i + 1 < chars.length then
              let c := chars[i+1]!
              s' := { s' with vm := { s'.vm with mem := s'.vm.mem.set! here c.toNat.toUInt8 } }
              here := here + 1
              i := i + 2
            else i := i + 1
          else if chars[i]! == '`' then
            if i + 1 < chars.length then
              let r := regNum chars[i+1]!
              if r < 32 then
                s' := { s' with vm := { s'.vm with mem := B4.setVal s'.vm.mem here (r * 4).toUInt32 } }
                here := here + 4
              i := i + 2
            else i := i + 1
          else
            while i < chars.length && chars[i]! > ' ' do i := i + 1
            let t := String.ofList (chars.take i |>.drop t_start)
            if let some op := getOp t then
              s' := { s' with vm := { s'.vm with mem := s'.vm.mem.set! here op } }
              here := here + 1
            else if t.startsWith "@" && t.length > 1 && (findLabel s'.labels (t.toList.drop 1 |> String.ofList)).isSome then
              let name := t.toList.drop 1 |> String.ofList
              let addr := (findLabel s'.labels name).get!
              s' := { s' with vm := { s'.vm with mem := s'.vm.mem.set! here 0x97 } }
              s' := { s' with vm := { s'.vm with mem := B4.setVal s'.vm.mem (here + 1) addr.toUInt32 } }
              s' := { s' with vm := { s'.vm with mem := s'.vm.mem.set! (here + 5) 0x93 } }
              here := here + 6
            else if t.startsWith "!" && t.length > 1 && (findLabel s'.labels (t.toList.drop 1 |> String.ofList)).isSome then
              let name := t.toList.drop 1 |> String.ofList
              let addr := (findLabel s'.labels name).get!
              s' := { s' with vm := { s'.vm with mem := s'.vm.mem.set! here 0x97 } }
              s' := { s' with vm := { s'.vm with mem := B4.setVal s'.vm.mem (here + 1) addr.toUInt32 } }
              s' := { s' with vm := { s'.vm with mem := s'.vm.mem.set! (here + 5) 0x95 } }
              here := here + 6
            else if let some val := toInt16 t then
              if t.length <= 2 || (t.startsWith "-" && t.length <= 3) then
                let v := if val < 0 then val + 256 else val
                s' := { s' with vm := { s'.vm with mem := s'.vm.mem.set! here v.toNat.toUInt8 } }
                here := here + 1
              else
                s' := { s' with vm := { s'.vm with mem := B4.setVal s'.vm.mem here (B4.fromInt32 val) } }
                here := here + 4
            else
              i := t_start
              break
        s' := { s' with vm := { s'.vm with mem := B4.setVal s'.vm.mem 124 here.toUInt32 } }
      else if token.startsWith "^" && token.length == 2 then
        let r := regNum token.toList[1]!
        if r < 32 then
          s' := { s' with vm := B4.cpush s'.vm 0 }
          s' := { s' with vm := B4.setIP s'.vm (B4.getVal s'.vm.mem (r * 4)).toNat }
          s' := { s' with vm := B4.setRST s'.vm 1 }
          s' := { s' with vm := B4.setRDB s'.vm 0 }
          s' := { s' with vm := B4.run s'.vm }
          s' := { s' with vm := B4.setRST s'.vm 1 }
      else if token.startsWith "!" && token.length == 2 then
        let r := regNum token.toList[1]!
        if r < 32 then
          let (v, ns) := B4.dpop s'.vm
          s' := { s' with vm := { ns with mem := B4.setVal ns.mem (r * 4) v } }
      else if token.startsWith "@" && token.length == 2 then
        let r := regNum token.toList[1]!
        if r < 32 then s' := { s' with vm := B4.dpush s'.vm (B4.getVal s'.vm.mem (r * 4)) }
      else if token.startsWith "?" && token.length == 2 && !(token.toList[1]!).isDigit then
        let r := regNum token.toList[1]!
        if r < 32 then
          let val := B4.getVal s'.vm.mem (r * 4)
          IO.println (padLeft (toB4Mat val) 8 '0')
      else if token.startsWith "?" && token.length > 1 && (token.toList[1]!).isDigit then
        let addr_str := String.ofList (token.toList.drop 1)
        match toNat16 addr_str with
        | some addr =>
          let mut out_str := ""
          for j in [0:16] do
            let b := s'.vm.mem.get! (addr + j)
            out_str := out_str ++ toMnemonic b ++ " "
          IO.println out_str.trimAscii.toString
        | none => s' := s'
      else if let some val := findLabel s'.labels token then
        s' := { s' with vm := B4.cpush s'.vm 0 }
        s' := { s' with vm := B4.setIP s'.vm val }
        s' := { s' with vm := B4.setRST s'.vm 1 }
        s' := { s' with vm := B4.setRDB s'.vm 0 }
        s' := { s' with vm := B4.run s'.vm }
        s' := { s' with vm := B4.setRST s'.vm 1 }
      else if let some op := getOp token then
        s' := { s' with vm := B4.runOp s'.vm op }
      else if let some val := toInt16 token then
        s' := { s' with vm := B4.dpush s'.vm (B4.fromInt32 val) }
    
    if s'.vm.ob != "" then
      IO.print s'.vm.ob
      s' := { s' with vm := { s'.vm with ob := "" } }
  
  loop s'

def main (_ : List String) : IO Unit := do
  let mut vm := B4.mkInitialState
  vm := B4.setRST vm 1
  vm := B4.setIP vm 0x100
  vm := { vm with mem := B4.setVal vm.mem 124 0x100 }
  loop { vm := vm, labels := [] }
