# Hexadecimal cells and the canonical private opcode table.
import Base
import ./vm.bend as V

def digit(+n: U32) -> Char:
  Chr{V.pick(U32.is_lt(n, 10), U32.add(n, 48), U32.add(n, 55))}

def hex_fixed(n: Nat, +x: U32, acc: String) -> String:
  match n:
    case 0n:
      acc
    case 1n+p:
      hex_fixed(p, U32.shrn(x, 4n), SCon{digit(U32.and(x, 15)), acc})

def trim_zero(s: String) -> String:
  match s:
    case SNil{}:
      "0"
    case SCon{Chr{48}, t}:
      trim_zero(t)
    case SCon{h, t}:
      SCon{h, t}

def hex(x: U32) -> String:
  trim_zero(hex_fixed(8n, x, ""))

def signed(neg: Bool, x: U32) -> String:
  match neg:
    case True{}:
      "-" ++ hex(V.negate(x))
    case False{}:
      hex(x)

def cell(+x: U32) -> String:
  signed(V.negative(x), x)

def stack_go(xs: List<&2, U32>, +acc: String) -> List<&2, U32> & String:
  match xs:
    case Nil{}:
      ([], acc)
    case +h <> t:
      V.pair(List<&2, U32>, String, List<&2, U32> & String,
        stack_go(t, Bool.pick(String, String.is_empty(acc), cell(h), cell(h) ++ " " ++ acc)),
        t => text => (h <> t, text))

def stack(xs: List<&2, U32>) -> List<&2, U32> & String:
  stack_go(xs, "")

def digit_value(+c: U32) -> U32:
  V.pick(Bool.and(U32.is_ge(c, 48), U32.is_le(c, 57)), U32.sub(c, 48),
    V.pick(Bool.and(U32.is_ge(c, 65), U32.is_le(c, 70)), U32.sub(c, 55),
      V.pick(Bool.and(U32.is_ge(c, 97), U32.is_le(c, 102)), U32.sub(c, 87), 16)))

def parsed(ok: Bool, x: U32) -> Maybe<&2, U32>:
  match ok:
    case True{}:
      Some{x}
    case False{}:
      None{}

def read_hex_go(s: String, x: U32, ok: Bool) -> Maybe<&2, U32>:
  match s:
    case SNil{}:
      parsed(ok, x)
    case SCon{Chr{c}, t}:
      +d = digit_value(c)
      read_hex_go(t, U32.add(U32.mul(x, 16), d), Bool.and(ok, U32.is_lt(d, 16)))

def negative_hex(n: Maybe<&2, U32>) -> Maybe<&2, U32>:
  match n:
    case None{}:
      None{}
    case Some{x}:
      Some{V.negate(x)}

def read_hex(s: String) -> Maybe<&2, U32>:
  match s:
    case SNil{}:
      None{}
    case SCon{Chr{45}, SNil{}}:
      None{}
    case SCon{Chr{45}, t}:
      negative_hex(read_hex_go(t, 0, True{}))
    case SCon{h, t}:
      read_hex_go(SCon{h, t}, 0, True{})

def register(s: String) -> Maybe<&2, U32>:
  match s:
    case SCon{Chr{+c}, SNil{}}:
      parsed(Bool.and(U32.is_ge(c, 64), U32.is_le(c, 95)), U32.sub(c, 64))
    case _:
      None{}

def normalize(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{Chr{35}, t}:
      ""
    case SCon{+h, t}:
      SCon{Bool.pick(Char, Char.is_space(h), ' ', h), normalize(t)}

def find_name(xs: List<&2, String>, +name: String, +index: U32) -> U32:
  match xs:
    case Nil{}:
      256
    case h <> t:
      V.pick(String.eq(h, name), index, find_name(t, name, U32.add(index, 1)))

def core_names() -> List<&2, String>:
  String.split("ad sb ml dv md sh an or xr nt eq lt du sw ov zp dc cd rb ri wb wi lb li rs ls jm hp h0 cl rt nx", ' ')

def opcode(s: String) -> U32:
  match s:
    case "..":
      0
    case "c0":
      192
    case "c1":
      193
    case "c2":
      246
    case "n1":
      247
    case "c4":
      248
    case "io":
      253
    case "db":
      254
    case "hl":
      255
    case +other:
      find_name(core_names(), other, 128)

def name_at(xs: List<&2, String>, +n: U32) -> String:
  match xs:
    case Nil{}:
      "??"
    case h <> t:
      Bool.pick(String, U32.is_zero(n), h, name_at(t, U32.sub(n, 1)))

def register_name(group: U32, r: U32) -> String:
  match group:
    case 0:
      "^" ++ SCon{Chr{U32.add(64, r)}, SNil{}}
    case 1:
      "@" ++ SCon{Chr{U32.add(64, r)}, SNil{}}
    case 2:
      "!" ++ SCon{Chr{U32.add(64, r)}, SNil{}}
    case _:
      "+" ++ SCon{Chr{U32.add(64, r)}, SNil{}}

def byte_other(reg: Bool, +x: U32) -> String:
  match reg:
    case True{}:
      register_name(U32.div(x, 32), U32.mod(x, 32))
    case False{}:
      Bool.pick(String, U32.is_lt(x, 160), name_at(core_names(), U32.sub(x, 128)), hex_fixed(2n, x, ""))

def byte(x: U32) -> String:
  match x:
    case 0:
      ".."
    case 192:
      "c0"
    case 193:
      "c1"
    case 246:
      "c2"
    case 247:
      "n1"
    case 248:
      "c4"
    case 253:
      "io"
    case 254:
      "db"
    case 255:
      "hl"
    case +other:
      byte_other(U32.is_lt(other, 128), other)
