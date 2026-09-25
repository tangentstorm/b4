# Mandelbrot on a swarm of B4 machines.
#
# 2^8 by 2^6 tiles, each a 16x16 block, is 2^14 machines. The picture
# prints one character per tile, taken from the center pixel.
# Coordinates are Q12.12 fixed point. b4 has no floats, and `sh` is a
# logical shift, so the bytecode subroutine fxmul finishes a signed
# product with an arithmetic shift built from integer ops.
# A tile inside the set runs every iteration. A tile outside escapes
# early and halts, so the machines do not take the same number of steps.
import Base
import ./vm.bend as V

def depth() -> Nat:
  14n

def addn(a: Nat, b: Nat) -> Nat:
  match a:
    case 0n:
      b
    case 1n+p:
      1n+addn(p, b)

def pow2n(+n: Nat) -> Nat:
  match n:
    case 0n:
      1n
    case 1n+p:
      addn(pow2n(p), pow2n(p))

def span(+d: Nat) -> U32:
  match d:
    case 0n:
      1
    case 1n+p:
      U32.mul(span(p), 2)

def code() -> List<&2, U32>:
  130 <> 140 <> 153 <> 225 <> 133 <> 247 <> 130 <> 140 <> 150 <> 20 <> 133 <> 141 <> 143 <> 141 <> 153 <> 244 <> 133 <> 135 <> 158 <> 151 <> 0 <> 3 <> 0 <> 0 <> 72 <> 73 <> 74 <> 192 <> 71 <> 192 <> 70 <> 41 <> 150 <> 16 <> 130 <> 38 <> 128 <> 150 <> 3 <> 130 <> 151 <> 0 <> 224 <> 255 <> 255 <> 128 <> 67 <> 42 <> 150 <> 16 <> 130 <> 39 <> 128 <> 150 <> 8 <> 130 <> 151 <> 0 <> 240 <> 255 <> 255 <> 128 <> 68 <> 192 <> 65 <> 192 <> 66 <> 192 <> 69 <> 37 <> 150 <> 16 <> 138 <> 137 <> 156 <> 57 <> 33 <> 33 <> 157 <> 0 <> 1 <> 0 <> 0 <> 34 <> 34 <> 157 <> 0 <> 1 <> 0 <> 0 <> 142 <> 142 <> 128 <> 151 <> 0 <> 64 <> 0 <> 0 <> 139 <> 137 <> 156 <> 6 <> 143 <> 143 <> 155 <> 27 <> 33 <> 34 <> 157 <> 0 <> 1 <> 0 <> 0 <> 193 <> 133 <> 36 <> 128 <> 66 <> 129 <> 35 <> 128 <> 65 <> 37 <> 193 <> 128 <> 69 <> 154 <> 69 <> 1 <> 0 <> 0 <> 37 <> 40 <> 148 <> 40 <> 193 <> 128 <> 72 <> 38 <> 193 <> 128 <> 70 <> 38 <> 150 <> 16 <> 139 <> 137 <> 156 <> 16 <> 192 <> 70 <> 39 <> 193 <> 128 <> 71 <> 39 <> 150 <> 16 <> 139 <> 137 <> 156 <> 8 <> 255 <> 154 <> 31 <> 1 <> 0 <> 0 <> 154 <> 29 <> 1 <> 0 <> 0 <> []

def poke(code: List<&2, U32>, mem: Array<U32>, +addr: U32) -> Array<U32>:
  match code:
    case Nil{}:
      mem
    case b <> rest:
      poke(rest, mem[addr] <- b, U32.add(addr, 1))

def blank() -> Array<U32>:
  [0 : U32^10n]

def machine(+code: List<&2, U32>, +index: U32) -> V.VM:
  tx = U32.and(index, 255)
  ty = U32.shrn(index, 8n)
  V.VM{poke(code, blank(), 256), tx <> ty <> [], [], 275,
    V.Control{V.Ru{}, V.Clear{}, 0}, ""}

type Swarm is Type:
  One{vm: V.VM}
  Both{left: Swarm, right: Swarm}

def grow(+code: List<&2, U32>, +d: Nat, +index: U32) -> Swarm:
  match d:
    case 0n:
      One{machine(code, index)}
    case 1n+p:
      step = span(p)
      left right = grow(code, p, index) grow(code, p, U32.add(index, step))
      Both{left, right}

def search(swarm: Swarm) -> Swarm:
  match swarm:
    case One{vm}:
      One{V.run(pow2n(19n), vm)}
    case Both{left, right}:
      a b = search(left) search(right)
      Both{a, b}

def center(vm: V.VM) -> U32:
  V.VM{mem, ds, cs, ip, control, out} = vm
  V.pair(Array<U32>, U32, U32, mem[904], mem => b => b)

def lo(c: Bool) -> String:
  match c:
    case True{}:
      " "
    case False{}:
      "+"

def top(c: Bool) -> String:
  match c:
    case True{}:
      "*"
    case False{}:
      "@"

def hi(c: Bool, n: U32) -> String:
  match c:
    case True{}:
      lo(U32.is_lt(n, 4))
    case False{}:
      top(U32.is_lt(n, 12))

def shade(+n: U32) -> String:
  hi(U32.is_lt(n, 8), n)

def end_row(c: Bool, glyph: String) -> String:
  match c:
    case True{}:
      glyph ++ "\n"
    case False{}:
      glyph

def cell(index: U32, n: U32) -> String:
  end_row(U32.is_eq(U32.and(index, 255), 255), shade(n))

def picture(swarm: Swarm, +index: U32, count: U32) -> String:
  match swarm:
    case One{vm}:
      cell(index, center(vm))
    case Both{left, right}:
      +half = U32.shrn(count, 1n)
      a b = picture(left, index, half) picture(right, U32.add(index, half), half)
      a ++ b

def main() -> IO(Unit):
  IO.print(picture(search!(grow(code(), depth(), 0)), 0, span(depth())))
