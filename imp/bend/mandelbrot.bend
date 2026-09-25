# Mandelbrot on a swarm of B4 machines.
#
# 2^8 by 2^6 tiles, each a 16x16 block, is 2^14 machines. The picture
# is written to mandelbrot.png: 4096 by 1024 grayscale pixels.
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

def tone(c: Bool, +n: U32) -> U32:
  match c:
    case True{}:
      0
    case False{}:
      U32.mul(n, 16)

def color(+n: U32) -> U32:
  tone(U32.is_eq(n, 16), n)

def map_color(xs: List<&2, U32>) -> List<&2, U32>:
  match xs:
    case Nil{}:
      Nil{}
    case b <> rest:
      color(b) <> map_color(rest)

def read_row(mem: Array<U32>, +addr: U32) -> Array<U32> & List<&2, U32>:
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[addr], mem => b0 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 1)], mem => b1 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 2)], mem => b2 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 3)], mem => b3 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 4)], mem => b4 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 5)], mem => b5 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 6)], mem => b6 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 7)], mem => b7 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 8)], mem => b8 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 9)], mem => b9 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 10)], mem => b10 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 11)], mem => b11 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 12)], mem => b12 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 13)], mem => b13 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 14)], mem => b14 =>
  V.pair(Array<U32>, U32, Array<U32> & List<&2, U32>, mem[U32.add(addr, 15)], mem => b15 =>
  (mem, b0 <> b1 <> b2 <> b3 <> b4 <> b5 <> b6 <> b7 <> b8 <> b9 <> b10 <> b11 <> b12 <> b13 <> b14 <> b15 <> Nil{})))))))))))))))))

def rows(n: Nat, mem: Array<U32>, +addr: U32) -> List<&2, List<&2, U32>>:
  match n:
    case 0n:
      Nil{}
    case 1n+p:
      V.pair(Array<U32>, List<&2, U32>, List<&2, List<&2, U32>>, read_row(mem, addr),
        mem2 => row => row <> rows(p, mem2, U32.add(addr, 16)))

def pixels(vm: V.VM) -> List<&2, List<&2, U32>>:
  V.VM{mem, ds, cs, ip, control, out} = vm
  rows(16n, mem, 768)

def join(xs: List<&2, List<&2, List<&2, U32>>>, ys: List<&2, List<&2, List<&2, U32>>>) ->
  List<&2, List<&2, List<&2, U32>>>:
  match xs:
    case Nil{}:
      ys
    case h <> t:
      h <> join(t, ys)

def collect(swarm: Swarm) -> List<&2, List<&2, List<&2, U32>>>:
  match swarm:
    case One{vm}:
      pixels(vm) <> Nil{}
    case Both{left, right}:
      a b = collect(left) collect(right)
      join(a, b)

def mix(odd: Bool, shifted: U32) -> U32:
  match odd:
    case True{}:
      U32.xor(shifted, 3988292384)
    case False{}:
      shifted

def crc_bit(+crc: U32) -> U32:
  mix(U32.is_eq(U32.and(crc, 1), 1), U32.shrn(crc, 1n))

def crc8(n: Nat, crc: U32) -> U32:
  match n:
    case 0n:
      crc
    case 1n+p:
      crc8(p, crc_bit(crc))

def crc_byte(+crc: U32, byte: U32) -> U32:
  crc8(8n, U32.xor(crc, byte))

def crc_list(xs: List<&2, U32>, crc: U32) -> U32:
  match xs:
    case Nil{}:
      crc
    case b <> rest:
      crc_list(rest, crc_byte(crc, b))

def mod_adler(c: Bool, +x: U32) -> U32:
  match c:
    case True{}:
      U32.sub(x, 65521)
    case False{}:
      x

def adler_byte(+state: U32, +byte: U32) -> U32:
  +a = U32.and(state, 65535)
  +b = U32.shrn(state, 16n)
  +sum = U32.add(a, byte)
  +a2 = mod_adler(U32.is_ge(sum, 65521), sum)
  +bsum = U32.add(b, a2)
  b2 = mod_adler(U32.is_ge(bsum, 65521), bsum)
  U32.or(U32.shln(b2, 16n), a2)

def adler_list(xs: List<&2, U32>, state: U32) -> U32:
  match xs:
    case Nil{}:
      state
    case b <> rest:
      adler_list(rest, adler_byte(state, b))

def be4(+n: U32) -> List<&2, U32>:
  U32.and(U32.shrn(n, 24n), 255) <> U32.and(U32.shrn(n, 16n), 255) <>
    U32.and(U32.shrn(n, 8n), 255) <> U32.and(n, 255) <> Nil{}

def header() -> List<&2, U32>:
  137 <> 80 <> 78 <> 71 <> 13 <> 10 <> 26 <> 10 <> 0 <> 0 <> 0 <> 13 <> 73 <> 72 <> 68 <> 82 <> 0 <> 0 <> 16 <> 0 <> 0 <> 0 <> 4 <> 0 <> 8 <> 0 <> 0 <> 0 <> 0 <> 68 <> 89 <> 195 <> 22 <> Nil{}

def iend() -> List<&2, U32>:
  0 <> 0 <> 0 <> 0 <> 73 <> 69 <> 78 <> 68 <> 174 <> 66 <> 96 <> 130 <> Nil{}

def wrote(p: File & Result<&1, &1, U32 & String, Unit>) -> IO(File):
  (file, result) = p
  match result:
    case Done{value}:
      IO.pure(File, file)
    case Fail{(code, message)}:
      IO.die(File, code, message)

def keep(act: IO(File & Result<&1, &1, U32 & String, Unit>)) -> IO(File):
  IO.bind(File & Result<&1, &1, U32 & String, Unit>, File, act, wrote)

def emit(file: File, bytes: List<&2, U32>) -> IO(File):
  keep(File.write_bytes(file, bytes))

def append(xs: List<&2, U32>, ys: List<&2, U32>) -> List<&2, U32>:
  match xs:
    case Nil{}:
      ys
    case h <> t:
      h <> append(t, ys)

def idat_name() -> List<&2, U32>:
  73 <> 68 <> 65 <> 84 <> Nil{}

def crc_idat() -> U32:
  crc_list(idat_name(), 4294967295)

def peeled(tile: List<&2, List<&2, U32>>, rows: List<&2, List<&2, U32>>, rest: List<&2, List<&2, List<&2, U32>>>) ->
  List<&2, List<&2, U32>> & List<&2, List<&2, List<&2, U32>>>:
  match tile:
    case Nil{}:
      (rows, rest)
    case row <> more:
      (row <> rows, more <> rest)

def peel_one(tile: List<&2, List<&2, U32>>, packed: List<&2, List<&2, U32>> & List<&2, List<&2, List<&2, U32>>>) ->
  List<&2, List<&2, U32>> & List<&2, List<&2, List<&2, U32>>>:
  (rows, rest) = packed
  peeled(tile, rows, rest)

def peel(tiles: List<&2, List<&2, List<&2, U32>>>) ->
  List<&2, List<&2, U32>> & List<&2, List<&2, List<&2, U32>>>:
  match tiles:
    case Nil{}:
      (Nil{}, Nil{})
    case tile <> more:
      peel_one(tile, peel(more))

def flatten(rows: List<&2, List<&2, U32>>) -> List<&2, U32>:
  match rows:
    case Nil{}:
      Nil{}
    case h <> t:
      append(map_color(h), flatten(t))

def block(scan: List<&2, U32>) -> List<&2, U32>:
  0 <> 1 <> 16 <> 254 <> 239 <> scan

def share_cons(b: U32, packed: List<&2, U32> & U32) -> List<&2, U32> & U32:
  (rest, crc) = packed
  (b <> rest, crc)

def share(xs: List<&2, U32>, crc: U32) -> List<&2, U32> & U32:
  match xs:
    case Nil{}:
      (Nil{}, crc)
    case +b <> rest:
      share_cons(b, share(rest, crc_byte(crc, b)))

def pack_frame(length: U32, packed: List<&2, U32> & U32) -> List<&2, U32>:
  (copy, crc) = packed
  append(be4(length), append(idat_name(), append(copy, be4(U32.xor(crc, 4294967295)))))

def frame(length: U32, payload: List<&2, U32>) -> List<&2, U32>:
  pack_frame(length, share(payload, crc_idat()))

def adler_cons(b: U32, packed: List<&2, U32> & U32) -> List<&2, U32> & U32:
  (rest, state) = packed
  (b <> rest, state)

def adler_walk(xs: List<&2, U32>, state: U32) -> List<&2, U32> & U32:
  match xs:
    case Nil{}:
      (Nil{}, state)
    case +b <> rest:
      adler_cons(b, adler_walk(rest, adler_byte(state, b)))

def scanline(rows: List<&2, List<&2, U32>>, adler: U32) -> List<&2, U32> & U32:
  adler_walk(0 <> flatten(rows), adler)

def hold(act: IO(File), adler: U32) -> IO(File & U32):
  IO.bind(File, File & U32, act, file => IO.pure(File & U32, (file, adler)))

def choose(first: Bool, file: File, scan: List<&2, U32>, adler: U32) -> IO(File & U32):
  match first:
    case True{}:
      hold(emit(file, frame(4104, append(120 <> 1 <> Nil{}, block(scan)))), adler)
    case False{}:
      hold(emit(file, frame(4102, block(scan))), adler)

def with_scan(first: Bool, file: File, packed: List<&2, U32> & U32) -> IO(File & U32):
  (scan, adler) = packed
  choose(first, file, scan, adler)

def write_scan(first: Bool, file: File, rows: List<&2, List<&2, U32>>, adler: U32) -> IO(File & U32):
  with_scan(first, file, scanline(rows, adler))

type Cont is Type:
  Cont{first: Bool, file: File, tiles: List<&2, List<&2, List<&2, U32>>>, adler: U32}

def finish(c: Cont) -> IO(File & U32):
  Cont{first, file, tiles, adler} = c
  IO.pure(File & U32, (file, adler))

def repack(got: File & U32, tiles: List<&2, List<&2, List<&2, U32>>>) -> IO(Cont):
  (file, adler) = got
  IO.pure(Cont, Cont{False{}, file, tiles, adler})

def open_rows(first: Bool, file: File, packed: List<&2, List<&2, U32>> & List<&2, List<&2, List<&2, U32>>>, adler: U32) -> IO(Cont):
  (rows, tiles) = packed
  IO.bind(File & U32, Cont, write_scan(first, file, rows, adler), got => repack(got, tiles))

def stage(c: Cont) -> IO(Cont):
  Cont{first, file, tiles, adler} = c
  open_rows(first, file, peel(tiles), adler)

def write_pys(left: Nat, c: Cont) -> IO(File & U32):
  match left:
    case 0n:
      finish(c)
    case 1n+p:
      IO.bind(Cont, File & U32, stage(c), next => write_pys(p, next))

def leaf_one(first: Bool, vm: V.VM, state: File & U32) -> IO(File & U32):
  (file, adler) = state
  write_pys(16n, Cont{first, file, pixels(vm) <> Nil{}, adler})

def leaf_both(first: Bool, left: Swarm, right: Swarm, state: File & U32) -> IO(File & U32):
  (file, adler) = state
  write_pys(16n, Cont{first, file, collect(Both{left, right}), adler})

def write_rows(+fuel: Nat, first: Bool, swarm: Swarm, state: File & U32) -> IO(File & U32):
  match fuel swarm:
    case 0n One{vm}:
      leaf_one(first, vm, state)
    case 1n+p One{vm}:
      leaf_one(first, vm, state)
    case 0n Both{left, right}:
      leaf_both(first, left, right, state)
    case 1n+p Both{left, right}:
      IO.bind(File & U32, File & U32, write_rows(p, first, left, state),
        got => write_rows(p, False{}, right, got))

def final_payload(adler: U32) -> List<&2, U32>:
  append(1 <> 0 <> 0 <> 255 <> 255 <> Nil{}, be4(adler))

def close_file(got: File & U32) -> IO(Unit):
  (file, adler) = got
  do IO<Unit>:
    file : File <- emit(file, frame(9, final_payload(adler)))
    file : File <- emit(file, iend())
    File.close(file)
    IO.print("wrote mandelbrot.png\n")

def main() -> IO(Unit):
  done = search!(grow(code(), depth(), 0))
  do IO<Unit>:
    file : File <- IO.try(File, File.open("mandelbrot.png", "w"))
    file : File <- emit(file, header())
    got : File & U32 <- write_rows(6n, True{}, done, (file, 1))
    close_file(got)
