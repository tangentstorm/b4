# 16384 B4 machines, one square each, in a window on the Windows desktop
# (WSLg forwards the X11 window). Register A counts laps. A click marks
# that machine.
import Base
import ./vm.bend as V

type Grid is Type:
  Cell{vm: V.VM}
  Quad{tl: Grid, tr: Grid, bl: Grid, br: Grid}

type Board is Type:
  Board{grid: Grid, x: U32, y: U32}

def fresh() -> V.VM:
  mem = [0 : U32^12n]
  mem = V.write_cell(mem, 124, 256)
  mem[256] <- 33
  mem[257] <- 193
  mem[258] <- 128
  mem[259] <- 65
  mem[260] <- 154
  mem[261] <- 0
  mem[262] <- 1
  mem[263] <- 0
  mem[264] <- 0
  V.VM{mem, [], [], 256, V.Control{V.Ru{}, V.Clear{}, 0}, ""}

def span(+n: Nat) -> U32:
  match n:
    case 0n:
      1
    case 1n+p:
      U32.add(span(p), span(p))

def grow(+d: Nat) -> Grid:
  match d:
    case 0n:
      Cell{fresh()}
    case 1n+p:
      a b c d = grow(p) grow(p) grow(p) grow(p)
      Quad{a, b, c, d}

def ink(on: Bool, +shade: U32) -> U32:
  match on:
    case True{}:
      16763904
    case False{}:
      U32.mul(U32.and(shade, 255), 65793)

def shade(vm: V.VM, on: Bool) -> V.VM & U32:
  V.VM{mem, ds, cs, ip, control, output} = vm
  V.pair(Array<U32>, U32, V.VM & U32, V.read_cell(mem, 4), mem => a => (
    (V.VM{mem, ds, cs, ip, control, output}, ink(on, a))
  ))

def painted(vm: V.VM, on: Bool) -> V.VM & U32:
  shade(V.tick(vm), on)

def as_cell(got: V.VM & U32) -> Grid & Image:
  (vm, col) = got
  (Cell{vm}, Pix{col})

def cell_at(vm: V.VM, +ox: U32, +oy: U32, +sx: U32, +sy: U32) -> Grid & Image:
  as_cell(painted(vm, Bool.and(U32.is_eq(ox, sx), U32.is_eq(oy, sy))))

def join(wa: Grid & Image, wb: Grid & Image, wc: Grid & Image, wd: Grid & Image) -> Grid & Image:
  (ga, ia) = wa
  (gb, ib) = wb
  (gc, ic) = wc
  (gd, id) = wd
  (Quad{ga, gb, gc, gd}, Qua{ia, ib, ic, id})

def frame(+d: Nat, +ox: U32, +oy: U32, +sx: U32, +sy: U32, grid: Grid) -> Grid & Image:
  match d grid:
    case 0n Cell{vm}:
      cell_at(vm, ox, oy, sx, sy)
    case 0n Quad{tl, tr, bl, br}:
      cell_at(fresh(), ox, oy, sx, sy)
    case 1n+p Cell{vm}:
      cell_at(vm, ox, oy, sx, sy)
    case 1n+p Quad{tl, tr, bl, br}:
      +step = span(p)
      wa wb wc wd = frame(p, ox, oy, sx, sy, tl)
        frame(p, U32.add(ox, step), oy, sx, sy, tr)
        frame(p, ox, U32.add(oy, step), sx, sy, bl)
        frame(p, U32.add(ox, step), U32.add(oy, step), sx, sy, br)
      join(wa, wb, wc, wd)

def seen(got: Grid & Image, +x: U32, +y: U32) -> Board & Image:
  (grid, image) = got
  (Board{grid, x, y}, image)

def view(board: Board) -> Board & Image:
  Board{grid, +x, +y} = board
  seen(frame(7n, 0, 0, x, y, grid), x, y)

def pick(down: Bool, +x: U32, +y: U32, board: Board) -> Board:
  match down:
    case True{}:
      Board{grid, ox, oy} = board
      Board{grid, U32.shrn(x, 2n), U32.shrn(y, 2n)}
    case False{}:
      board

def react(events: List<&1, Event>, board: Board) -> IO(Maybe<&1, Board>):
  match events:
    case Nil{}:
      IO.pure(Maybe<&1, Board>, Some{board})
    case Close{} <> rest:
      IO.pure(Maybe<&1, Board>, None{})
    case Mouse{x, y, button, down} <> rest:
      react(rest, pick(down, x, y, board))
    case other <> rest:
      react(rest, board)

def main() -> IO(Unit):
  App.run(~Board, ~App{view, react}, "b4 machines", 512, 512, Board{grow(7n), 0, 0})
