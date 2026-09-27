# 16384 B4 machines in a window on the Windows desktop. Each square is one
# machine. A click attaches the b4i line (typed in this process) to that
# machine. @@ holds its id, y * 128 + x.
import Base
import ./vm.bend as V
import ./text.bend as T
import ./b4i.bend as I

def Poll.stdin(+max: U32) -> IO(String):
  import "./poll_stdin.c"
  import "./poll_stdin.js"

type Grid is Type:
  Cell{vm: V.VM}
  Quad{tl: Grid, tr: Grid, bl: Grid, br: Grid}

type Board is Type:
  Board{grid: Grid, x: U32, y: U32}

type Way is Data:
  TL{}
  TR{}
  BL{}
  BR{}

type Path is Type:
  End{}
  Go{way: Way, rest: Path}

type Desk is Type:
  Desk{board: Board, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String}

def machine_id(+x: U32, +y: U32) -> U32:
  U32.add(U32.mul(y, 128), x)

def fresh(+id: U32) -> V.VM:
  mem = [0 : U32^12n]
  mem = V.write_cell(mem, 0, id)
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

def grow(+d: Nat, +ox: U32, +oy: U32) -> Grid:
  match d:
    case 0n:
      Cell{fresh(machine_id(ox, oy))}
    case 1n+p:
      +step = span(p)
      a b c d = grow(p, ox, oy) grow(p, U32.add(ox, step), oy) grow(p, ox, U32.add(oy, step)) grow(p, U32.add(ox, step), U32.add(oy, step))
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

def sink(vm: V.VM) -> U32:
  V.VM{mem, ds, cs, ip, control, output} = vm
  0

def discard(grid: Grid) -> U32:
  match grid:
    case Cell{vm}:
      sink(vm)
    case Quad{tl, tr, bl, br}:
      a b c d = discard(tl) discard(tr) discard(bl) discard(br)
      U32.add(U32.add(a, b), U32.add(c, d))

def frame(+d: Nat, +ox: U32, +oy: U32, +sx: U32, +sy: U32, grid: Grid) -> Grid & Image:
  match d grid:
    case 0n Cell{vm}:
      cell_at(vm, ox, oy, sx, sy)
    case 0n Quad{tl, tr, bl, br}:
      gone = discard(Quad{tl, tr, bl, br})
      cell_at(fresh(0), ox, oy, sx, sy)
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

def pick(down: Bool, +px: U32, +py: U32, board: Board) -> Board:
  match down:
    case True{}:
      Board{grid, ox, oy} = board
      Board{grid, U32.shrn(px, 2n), U32.shrn(py, 2n)}
    case False{}:
      board

def drop_events(events: List<&1, Event>) -> U32:
  match events:
    case Nil{}:
      0
    case other <> rest:
      drop_events(rest)

def react(events: List<&1, Event>, board: Board) -> Board & Bool:
  match events:
    case Nil{}:
      (board, False{})
    case Close{} <> rest:
      gone = drop_events(rest)
      (board, True{})
    case Mouse{x, y, button, down} <> rest:
      react(rest, pick(down, x, y, board))
    case other <> rest:
      react(rest, board)

def bit_at(+n: U32, +shift: Nat) -> Bool:
  U32.is_eq(U32.and(U32.shrn(n, shift), 1), 1)

def way(hy: Bool, hx: Bool) -> Way:
  match hy hx:
    case False{} False{}:
      TL{}
    case False{} True{}:
      TR{}
    case True{} False{}:
      BL{}
    case True{} True{}:
      BR{}

def bits(+n: Nat, +x: U32, +y: U32) -> Path:
  match n:
    case 0n:
      End{}
    case 1n+p:
      Go{way(bit_at(y, p), bit_at(x, p)), bits(p, x, y)}

def join_tl(got: V.VM & Grid, tr: Grid, bl: Grid, br: Grid) -> V.VM & Grid:
  (vm, tl) = got
  (vm, Quad{tl, tr, bl, br})

def join_tr(got: V.VM & Grid, tl: Grid, bl: Grid, br: Grid) -> V.VM & Grid:
  (vm, tr) = got
  (vm, Quad{tl, tr, bl, br})

def join_bl(got: V.VM & Grid, tl: Grid, tr: Grid, br: Grid) -> V.VM & Grid:
  (vm, bl) = got
  (vm, Quad{tl, tr, bl, br})

def join_br(got: V.VM & Grid, tl: Grid, tr: Grid, bl: Grid) -> V.VM & Grid:
  (vm, br) = got
  (vm, Quad{tl, tr, bl, br})

def walk(path: Path, grid: Grid) -> V.VM & Grid:
  match path grid:
    case End{} Cell{vm}:
      (vm, Cell{fresh(0)})
    case End{} Quad{tl, tr, bl, br}:
      gone = discard(Quad{tl, tr, bl, br})
      (fresh(0), Cell{fresh(0)})
    case Go{TL{}, rest} Quad{tl, tr, bl, br}:
      join_tl(walk(rest, tl), tr, bl, br)
    case Go{TR{}, rest} Quad{tl, tr, bl, br}:
      join_tr(walk(rest, tr), tl, bl, br)
    case Go{BL{}, rest} Quad{tl, tr, bl, br}:
      join_bl(walk(rest, bl), tl, tr, br)
    case Go{BR{}, rest} Quad{tl, tr, bl, br}:
      join_br(walk(rest, br), tl, tr, bl)
    case Go{way, rest} Cell{vm}:
      gone = sink(vm)
      (fresh(0), Cell{fresh(0)})

def put_tl(child: Grid, tr: Grid, bl: Grid, br: Grid) -> Grid:
  Quad{child, tr, bl, br}

def put_tr(tl: Grid, child: Grid, bl: Grid, br: Grid) -> Grid:
  Quad{tl, child, bl, br}

def put_bl(tl: Grid, tr: Grid, child: Grid, br: Grid) -> Grid:
  Quad{tl, tr, child, br}

def put_br(tl: Grid, tr: Grid, bl: Grid, child: Grid) -> Grid:
  Quad{tl, tr, bl, child}

def put(path: Path, grid: Grid, vm: V.VM) -> Grid:
  match path grid:
    case End{} Cell{old}:
      gone = sink(old)
      Cell{vm}
    case End{} Quad{tl, tr, bl, br}:
      gone = discard(Quad{tl, tr, bl, br})
      Cell{vm}
    case Go{TL{}, rest} Quad{tl, tr, bl, br}:
      put_tl(put(rest, tl, vm), tr, bl, br)
    case Go{TR{}, rest} Quad{tl, tr, bl, br}:
      put_tr(tl, put(rest, tr, vm), bl, br)
    case Go{BL{}, rest} Quad{tl, tr, bl, br}:
      put_bl(tl, tr, put(rest, bl, vm), br)
    case Go{BR{}, rest} Quad{tl, tr, bl, br}:
      put_br(tl, tr, bl, put(rest, br, vm))
    case Go{way, rest} Cell{old}:
      gone = sink(old)
      Cell{vm}

def stamp(vm: V.VM, +id: U32) -> V.VM:
  V.VM{mem, ds, cs, ip, control, output} = vm
  V.VM{V.write_cell(mem, 0, id), ds, cs, ip, control, output}

def from_shell(shell: I.Shell, grid: Grid, +x: U32, +y: U32, prior: String, pending: String) -> Desk:
  I.Shell{vm, labels, assembling, quit, report} = shell
  Desk{Board{put(bits(7n, x, y), grid, stamp(vm, machine_id(x, y))), x, y}, labels, assembling, prior ++ report, quit, pending}

def command(got: V.VM & Grid, text: String, labels: Map<&2, U32>, assembling: Bool, +x: U32, +y: U32, prior: String, pending: String) -> Desk:
  (vm, grid) = got
  from_shell(I.line(text, I.Shell{vm, labels, assembling, False{}, ""}), grid, x, y, prior, pending)

def say(desk: Desk, text: String) -> Desk:
  Desk{board, labels, assembling, report, quit, pending} = desk
  Desk{board, labels, assembling, report ++ text, quit, pending}

def apply_line(text: String, desk: Desk) -> Desk:
  Desk{board, +labels, +assembling, report, quit, pending} = desk
  Board{grid, +x, +y} = board
  command(walk(bits(7n, x, y), grid), text, labels, assembling, x, y, report, pending)

def route(words: List<&2, String>, text: String, desk: Desk) -> Desk:
  match words:
    case "/q" <> Nil{}:
      Desk{board, labels, assembling, report, quit, pending} = desk
      Desk{board, labels, assembling, report, True{}, pending}
    case _:
      apply_line(text, desk)

def apply_norm(+text: String, desk: Desk) -> Desk:
  route(String.split(text, ' '), text, desk)

def on_line(text: String, desk: Desk) -> Desk:
  apply_norm(T.normalize(text), desk)

def pump(chunk: String, desk: Desk) -> Desk:
  match chunk desk:
    case SNil{} desk:
      desk
    case SCon{Chr{4}, SNil{}} Desk{board, labels, assembling, report, quit, pending}:
      Desk{board, labels, assembling, report, True{}, pending}
    case SCon{Chr{10}, tail} Desk{board, labels, assembling, report, quit, pending}:
      pump(tail, on_line(String.reverse(pending), Desk{board, labels, assembling, report, quit, ""}))
    case SCon{c, tail} Desk{board, labels, assembling, report, quit, pending}:
      pump(tail, Desk{board, labels, assembling, report, quit, SCon{c, pending}})

def keep_board(closed: Bool, board: Board, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String) -> Desk:
  match closed:
    case True{}:
      Board{grid, x, y} = board
      gone = discard(grid)
      Desk{Board{Cell{fresh(0)}, 0, 0}, labels, assembling, report, True{}, pending}
    case False{}:
      Desk{board, labels, assembling, report, quit, pending}

def heard(got: Board & Bool, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String) -> Desk:
  (board, closed) = got
  keep_board(closed, board, labels, assembling, report, quit, pending)

def flush(empty: Bool, text: String) -> IO(Unit):
  match empty:
    case True{}:
      do IO<Unit>:
        return Unit{}
    case False{}:
      IO.write(text)

type Stage is Type:
  Stage{window: Window, board: Board, image: Image, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String}

def staged(got: Board & Image, window: Window, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String) -> Stage:
  (board, image) = got
  Stage{window, board, image, labels, assembling, report, quit, pending}

def typed(desk: Desk, window: Window, image: Image) -> Stage:
  Desk{board, labels, assembling, report, quit, pending} = desk
  Stage{window, board, image, labels, assembling, report, quit, pending}

def next_stage(shown: Window & Image & List<&1, Event>, board: Board, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String, chunk: String) -> Stage:
  (window, image, events) = shown
  gone = Image.drop(image)
  desk = heard(react(events, board), labels, assembling, report, quit, pending)
  typed(pump(chunk, desk), window, Pix{0})

def with_view(stage: Stage) -> Stage:
  Stage{window, board, image, labels, assembling, report, quit, pending} = stage
  dropped = Image.drop(image)
  staged(view(board), window, labels, assembling, report, quit, pending)

@unsafe def loop(stage: Stage) -> IO(Unit):
  match stage:
    case Stage{window, Board{grid, x, y}, image, labels, assembling, report, True{}, pending}:
      gone = discard(grid)
      dropped = Image.drop(image)
      do IO<Unit>:
        flush(String.is_empty(report), report)
        Window.close(window)
    case Stage{window, board, image, labels, assembling, +report, False{}, pending}:
      do IO<Unit>:
        shown : Window & Image & List<&1, Event> <- Window.frame(window, image)
        chunk : String <- Poll.stdin(256)
        loop(with_view(next_stage(shown, board, labels, assembling, report, False{}, pending, chunk)))

def main() -> IO(Unit):
  do IO<Unit>:
    IO.write("b4 machines. click a square. @@ is that machine's id. ?@ prints it. /q quits.\n")
    window : Window <- IO.try(Window, Window.open("b4 machines", 512, 512))
    loop(staged(view(Board{grow(7n, 0, 0), 0, 0}), window, Map.new(&2, U32), False{}, "", False{}, ""))
