# 16384 B4 machines in a window on the Windows desktop. The left half is
# the grid: each square is one machine. A click there attaches the repl.
# The right half is that repl. Keys typed in the window go to the gold
# square. In this window @@ stores that machine's id, y * 128 + x.
# /all sends one b4i line to every machine. Letters arrive lower case, so
# a two-character register word such as ?a or !a is read as ?A or !A.
# ?d ?c ?s and ?i stay queries.
import Base
import ./vm.bend as V
import ./text.bend as T
import ./b4i.bend as I
import ./font.bend as F

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
  Desk{board: Board, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String, input: String, lines: List<&2, String>}

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

def as_u32(n: Nat) -> U32:
  match n:
    case 0n:
      0
    case 1n+p:
      U32.add(as_u32(p), 1)

def as_nat(n: U32) -> Nat:
  match n:
    case 0:
      0n
    case 1:
      1n
    case 2:
      2n
    case 4:
      4n
    case _:
      0n

def inkbit(on: Bool, fg: U32, bg: U32) -> U32:
  match on:
    case True{}:
      fg
    case False{}:
      bg

def gpix(+n: Nat, +ox: Nat, +oy: Nat, +ch: U32, +fg: U32, +bg: U32) -> Image:
  match n:
    case 0n:
      Pix{inkbit(U32.is_ne(U32.and(U32.shrn(F.glyph(ch, as_u32(oy)), ox), 1), 0), fg, bg)}
    case 1n+p:
      +step = span(p)
      a b c d = gpix(p, ox, oy, ch, fg, bg)
        gpix(p, Nat.add(ox, as_nat(step)), oy, ch, fg, bg)
        gpix(p, ox, Nat.add(oy, as_nat(step)), ch, fg, bg)
        gpix(p, Nat.add(ox, as_nat(step)), Nat.add(oy, as_nat(step)), ch, fg, bg)
      Qua{a, b, c, d}

def glyph_image(+ch: U32) -> Image:
  match ch:
    case 32:
      Pix{1713186}
    case 1:
      Pix{8383487}
    case _:
      gpix(3n, 0n, 0n, ch, 15263976, 1713186)

def drop_text(text: String) -> String:
  match text:
    case SNil{}:
      ""
    case SCon{c, rest}:
      drop_text(rest)

def drop_lines(lines: List<&2, String>) -> U32:
  match lines:
    case Nil{}:
      0
    case h <> t:
      gone = drop_text(h)
      drop_lines(t)

def str_at(text: String, n: U32) -> U32:
  match text n:
    case SNil{} _:
      32
    case SCon{Chr{c}, rest} 0:
      gone = drop_text(rest)
      c
    case SCon{Chr{c}, rest} k:
      str_at(rest, U32.sub(k, 1))

def line_at(lines: List<&2, String>, row: U32) -> String:
  match lines row:
    case Nil{} _:
      ""
    case h <> t 0:
      gone = drop_lines(t)
      h
    case h <> t k:
      gone = drop_text(h)
      line_at(t, U32.sub(k, 1))

# Row 63 is the entry line. Above it, line 0 (the newest) sits on row 62.
def on_entry(+row: U32) -> Bool:
  U32.is_eq(row, 63)

def above(+row: U32) -> U32:
  U32.sub(62, row)

def show_entry(here: Bool, +col: U32, +row: U32, +lines: List<&2, String>, +entry: String) -> U32:
  match here:
    case True{}:
      gone = drop_lines(lines)
      str_at(entry, col)
    case False{}:
      quiet = drop_text(entry)
      str_at(line_at(lines, above(row)), col)

def cell_char(+col: U32, +row: U32, +lines: List<&2, String>, +entry: String) -> U32:
  show_entry(on_entry(row), col, row, lines, entry)

def console(+n: Nat, +ox: U32, +oy: U32, +lines: List<&2, String>, +entry: String) -> Image:
  match n:
    case 0n:
      Pix{1713186}
    case 3n:
      glyph_image(cell_char(U32.shrn(ox, 3n), U32.shrn(oy, 3n), lines, entry))
    case 1n+p:
      +step = span(p)
      a b c d = console(p, ox, oy, lines, entry)
        console(p, U32.add(ox, step), oy, lines, entry)
        console(p, ox, U32.add(oy, step), lines, entry)
        console(p, U32.add(ox, step), U32.add(oy, step), lines, entry)
      Qua{a, b, c, d}

def drop_events(events: List<&1, Event>) -> U32:
  match events:
    case Nil{}:
      0
    case other <> rest:
      drop_events(rest)

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

def from_shell(shell: I.Shell, grid: Grid, +x: U32, +y: U32, prior: String, pending: String, input: String, lines: List<&2, String>) -> Desk:
  I.Shell{vm, labels, assembling, quit, report} = shell
  Desk{Board{put(bits(7n, x, y), grid, stamp(vm, machine_id(x, y))), x, y}, labels, assembling, prior ++ report, quit, pending, input, lines}

def command(got: V.VM & Grid, text: String, labels: Map<&2, U32>, assembling: Bool, +x: U32, +y: U32, prior: String, pending: String, input: String, lines: List<&2, String>) -> Desk:
  (vm, grid) = got
  from_shell(I.line(text, I.Shell{vm, labels, assembling, False{}, ""}), grid, x, y, prior, pending, input, lines)

type Cast is Type:
  Cast{grid: Grid, labels: Map<&2, U32>, report: String, quit: Bool, hit: Bool}

def kept(on: Bool, report: String) -> String:
  match on:
    case True{}:
      report
    case False{}:
      drop_text(report)

def cast_shell(shell: I.Shell, +id: U32, +on: Bool) -> Cast:
  I.Shell{vm, labels, assembling, quit, report} = shell
  Cast{Cell{stamp(vm, id)}, labels, kept(on, report), quit, on}

def cast_cell(vm: V.VM, +text: String, +labels: Map<&2, U32>, +id: U32, on: Bool) -> Cast:
  cast_shell(I.line(text, I.Shell{vm, labels, False{}, False{}, ""}), id, on)

def take_map(hit: Bool, mine: Map<&2, U32>, other: Map<&2, U32>) -> Map<&2, U32>:
  match hit:
    case True{}:
      mine
    case False{}:
      other

def glue(a: Cast, b: Cast, c: Cast, d: Cast) -> Cast:
  Cast{ga, la, ra, qa, +ha} = a
  Cast{gb, lb, rb, qb, +hb} = b
  Cast{gc, lc, rc, qc, +hc} = c
  Cast{gd, ld, rd, qd, +hd} = d
  Cast{Quad{ga, gb, gc, gd}, take_map(ha, la, take_map(hb, lb, take_map(hc, lc, ld))), ra ++ rb ++ rc ++ rd, Bool.or(Bool.or(qa, qb), Bool.or(qc, qd)), Bool.or(Bool.or(ha, hb), Bool.or(hc, hd))}

def cast_grid(+d: Nat, +ox: U32, +oy: U32, +sx: U32, +sy: U32, grid: Grid, +text: String, +labels: Map<&2, U32>) -> Cast:
  match d grid:
    case 0n Cell{vm}:
      cast_cell(vm, text, labels, machine_id(ox, oy), Bool.and(U32.is_eq(ox, sx), U32.is_eq(oy, sy)))
    case 0n Quad{tl, tr, bl, br}:
      gone = discard(Quad{tl, tr, bl, br})
      cast_cell(fresh(0), text, labels, 0, False{})
    case 1n+p Cell{vm}:
      cast_cell(vm, text, labels, machine_id(ox, oy), Bool.and(U32.is_eq(ox, sx), U32.is_eq(oy, sy)))
    case 1n+p Quad{tl, tr, bl, br}:
      +step = span(p)
      glue(cast_grid(p, ox, oy, sx, sy, tl, text, labels), cast_grid(p, U32.add(ox, step), oy, sx, sy, tr, text, labels), cast_grid(p, ox, U32.add(oy, step), sx, sy, bl, text, labels), cast_grid(p, U32.add(ox, step), U32.add(oy, step), sx, sy, br, text, labels))

def join_words(words: List<&2, String>) -> String:
  match words:
    case Nil{}:
      ""
    case h <> Nil{}:
      h
    case h <> t:
      h ++ " " ++ join_words(t)

def taken(got: Cast, +x: U32, +y: U32, prior: String, quit: Bool, pending: String, input: String, lines: List<&2, String>) -> Desk:
  Cast{grid, labels, report, stop, hit} = got
  Desk{Board{grid, x, y}, labels, False{}, prior ++ report ++ "all 16384\n", Bool.or(quit, stop), pending, input, lines}

def broadcast(text: String, desk: Desk) -> Desk:
  Desk{board, +labels, assembling, report, quit, pending, input, lines} = desk
  Board{grid, +x, +y} = board
  taken(cast_grid(7n, 0, 0, x, y, grid, text, labels), x, y, report, quit, pending, input, lines)

def say(desk: Desk, text: String) -> Desk:
  Desk{board, labels, assembling, report, quit, pending, input, lines} = desk
  Desk{board, labels, assembling, report ++ text, quit, pending, input, lines}

def apply_line(text: String, desk: Desk) -> Desk:
  Desk{board, +labels, +assembling, report, quit, pending, input, lines} = desk
  Board{grid, +x, +y} = board
  command(walk(bits(7n, x, y), grid), text, labels, assembling, x, y, report, pending, input, lines)

def route(words: List<&2, String>, text: String, desk: Desk) -> Desk:
  match words:
    case "/q" <> Nil{}:
      Desk{board, labels, assembling, report, quit, pending, input, lines} = desk
      Desk{board, labels, assembling, report, True{}, pending, input, lines}
    case "/all" <> Nil{}:
      say(desk, "usage: /all command\n")
    case "/all" <> rest:
      broadcast(join_words(rest), desk)
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
    case SCon{Chr{4}, SNil{}} Desk{board, labels, assembling, report, quit, pending, input, lines}:
      Desk{board, labels, assembling, report, True{}, pending, input, lines}
    case SCon{Chr{10}, tail} Desk{board, labels, assembling, report, quit, pending, input, lines}:
      pump(tail, on_line(String.reverse(pending), Desk{board, labels, assembling, report, quit, "", input, lines}))
    case SCon{c, tail} Desk{board, labels, assembling, report, quit, pending, input, lines}:
      pump(tail, Desk{board, labels, assembling, report, quit, SCon{c, pending}, input, lines})

def keep_board(closed: Bool, board: Board, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String, input: String, lines: List<&2, String>) -> Desk:
  match closed:
    case True{}:
      Board{grid, x, y} = board
      gone = discard(grid)
      quiet = drop_text(input)
      rest = drop_lines(lines)
      Desk{Board{Cell{fresh(0)}, 0, 0}, labels, assembling, report, True{}, pending, "", Nil{}}
    case False{}:
      Desk{board, labels, assembling, report, quit, pending, input, lines}

def flush(empty: Bool, text: String) -> IO(Unit):
  match empty:
    case True{}:
      do IO<Unit>:
        return Unit{}
    case False{}:
      IO.write(text)

def take_lines(n: Nat, lines: List<&2, String>) -> List<&2, String>:
  match n lines:
    case 0n extra:
      gone = drop_lines(extra)
      Nil{}
    case 1n+p Nil{}:
      Nil{}
    case 1n+p h <> t:
      h <> take_lines(p, t)

def keep(lines: List<&2, String>) -> List<&2, String>:
  take_lines(40n, lines)

def finish_line(empty: Bool, acc: String, lines: List<&2, String>) -> List<&2, String>:
  match empty:
    case True{}:
      gone = drop_text(acc)
      lines
    case False{}:
      String.reverse(acc) <> lines

# Walk a report. The first argument shrinks. A full row (64 cells) wraps.
def absorb(text: String, +acc: String, lines: List<&2, String>, n: Nat) -> List<&2, String>:
  match text n:
    case SNil{} _:
      finish_line(String.is_empty(acc), acc, lines)
    case SCon{Chr{10}, rest} _:
      absorb(rest, "", String.reverse(acc) <> lines, 64n)
    case SCon{c, rest} 0n:
      absorb(rest, SCon{c, ""}, String.reverse(acc) <> lines, 63n)
    case SCon{c, rest} 1n+p:
      absorb(rest, SCon{c, acc}, lines, p)

def store(empty: Bool, board: Board, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String, input: String, lines: List<&2, String>) -> Desk:
  match empty:
    case True{}:
      gone = drop_text(report)
      Desk{board, labels, assembling, "", quit, pending, input, lines}
    case False{}:
      Desk{board, labels, assembling, "", quit, pending, input, keep(absorb(report, "", lines, 64n))}

def commit(desk: Desk) -> Desk:
  Desk{board, labels, assembling, +report, quit, pending, input, lines} = desk
  store(String.is_empty(report), board, labels, assembling, report, quit, pending, input, lines)

def is_intro(p: U32) -> Bool:
  match p:
    case 33:
      True{}
    case 43:
      True{}
    case 58:
      True{}
    case 63:
      True{}
    case 64:
      True{}
    case 94:
      True{}
    case 96:
      True{}
    case _:
      False{}

# ?d ?c ?s ?i are queries. Every other one-letter register word is upper case.
def query_letter(c: Char) -> Bool:
  match c:
    case Chr{100}:
      True{}
    case Chr{99}:
      True{}
    case Chr{115}:
      True{}
    case Chr{105}:
      True{}
    case _:
      False{}

def keep_query(hold: Bool, p: U32, c: Char) -> String:
  match hold:
    case True{}:
      SCon{Chr{p}, SCon{c, ""}}
    case False{}:
      SCon{Chr{p}, SCon{Char.to_upper(c), ""}}

def lift_pair(mark: Bool, +p: U32, +c: Char) -> String:
  match mark:
    case False{}:
      SCon{Chr{p}, SCon{c, ""}}
    case True{}:
      keep_query(Bool.and(U32.is_eq(p, 63), query_letter(c)), p, c)

def lift_word(word: String) -> String:
  match word:
    case "/c":
      "/C"
    case SCon{Chr{+p}, SCon{c, SNil{}}}:
      lift_pair(is_intro(p), p, c)
    case _:
      word

def lift_words(words: List<&2, String>) -> List<&2, String>:
  match words:
    case Nil{}:
      Nil{}
    case h <> t:
      lift_word(h) <> lift_words(t)

def lift_line(text: String) -> String:
  join_words(lift_words(String.split(text, ' ')))

def enter(desk: Desk) -> Desk:
  Desk{board, labels, assembling, report, quit, pending, input, lines} = desk
  +shown = lift_line(input)
  on_line(shown, Desk{board, labels, assembling, report, quit, pending, "", keep(("> " ++ shown) <> lines)})

def chop(text: String) -> String:
  match text:
    case SNil{}:
      ""
    case SCon{c, SNil{}}:
      ""
    case SCon{c, rest}:
      SCon{c, chop(rest)}

def backspace(desk: Desk) -> Desk:
  Desk{board, labels, assembling, report, quit, pending, input, lines} = desk
  Desk{board, labels, assembling, report, quit, pending, chop(input), lines}

def room(n: Nat, text: String) -> Bool:
  match n text:
    case 0n extra:
      gone = drop_text(extra)
      False{}
    case 1n+p SNil{}:
      True{}
    case 1n+p SCon{c, rest}:
      room(p, rest)

def printable(+code: U32) -> Bool:
  Bool.and(U32.is_lt(31, code), U32.is_lt(code, 127))

def write_char(ok: Bool, code: U32, desk: Desk) -> Desk:
  match ok:
    case False{}:
      desk
    case True{}:
      Desk{board, labels, assembling, report, quit, pending, input, lines} = desk
      Desk{board, labels, assembling, report, quit, pending, input ++ SCon{Chr{code}, ""}, lines}

def type_char(ok: Bool, +code: U32, desk: Desk) -> Desk:
  match ok:
    case False{}:
      desk
    case True{}:
      Desk{board, labels, assembling, report, quit, pending, +input, lines} = desk
      write_char(room(60n, input), code, Desk{board, labels, assembling, report, quit, pending, input, lines})

def keyed(+code: U32, desk: Desk) -> Desk:
  match code:
    case 13:
      enter(desk)
    case 127:
      backspace(desk)
    case _:
      type_char(printable(code), code, desk)

def typing(down: Bool, +code: U32, desk: Desk) -> Desk:
  match down:
    case False{}:
      desk
    case True{}:
      keyed(code, desk)

def place(stay: Bool, +nx: U32, +ny: U32, grid: Grid, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String, input: String, lines: List<&2, String>) -> Desk:
  match stay:
    case True{}:
      Desk{Board{grid, nx, ny}, labels, assembling, report, quit, pending, input, lines}
    case False{}:
      Desk{Board{grid, nx, ny}, labels, assembling, report, quit, pending, input, keep(("@@ " ++ T.hex_fixed(8n, machine_id(nx, ny), "")) <> lines)}

def retarget(+nx: U32, +ny: U32, desk: Desk) -> Desk:
  Desk{board, labels, assembling, report, quit, pending, input, lines} = desk
  Board{grid, x, y} = board
  place(Bool.and(U32.is_eq(nx, x), U32.is_eq(ny, y)), nx, ny, grid, labels, assembling, report, quit, pending, input, lines)

def aim(panel: Bool, +px: U32, +py: U32, desk: Desk) -> Desk:
  match panel:
    case False{}:
      desk
    case True{}:
      retarget(U32.shrn(px, 2n), U32.shrn(py, 2n), desk)

def click(down: Bool, +px: U32, +py: U32, desk: Desk) -> Desk:
  match down:
    case False{}:
      desk
    case True{}:
      aim(U32.is_lt(px, 512), px, py, desk)

def shut(desk: Desk) -> Desk:
  Desk{board, labels, assembling, report, quit, pending, input, lines} = desk
  keep_board(True{}, board, labels, assembling, report, quit, pending, input, lines)

def fold(events: List<&1, Event>, desk: Desk) -> Desk:
  match events:
    case Nil{}:
      desk
    case Close{} <> rest:
      gone = drop_events(rest)
      shut(desk)
    case Mouse{+x, +y, button, down} <> rest:
      fold(rest, click(down, x, y, desk))
    case Key{+code, down} <> rest:
      fold(rest, typing(down, code, desk))
    case other <> rest:
      fold(rest, desk)

def prompt(input: String) -> String:
  "> " ++ input ++ SCon{Chr{1}, ""}

def beside(got: Grid & Image, +x: U32, +y: U32, +lines: List<&2, String>, +entry: String) -> Board & Image:
  (grid, tiles) = got
  (Board{grid, x, y}, Qua{tiles, console(9n, 0, 0, lines, entry), Pix{1713186}, Pix{1713186}})

def scene(board: Board, +lines: List<&2, String>, +entry: String) -> Board & Image:
  Board{grid, +x, +y} = board
  beside(frame(7n, 0, 0, x, y, grid), x, y, lines, entry)

def hello() -> List<&2, String>:
  "@@ 00000000" <> ("/all broadcasts. /q quits." <> ("click a square, then type." <> Nil{}))

type Stage is Type:
  Stage{window: Window, board: Board, image: Image, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String, input: String, lines: List<&2, String>}

def staged(got: Board & Image, window: Window, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String, input: String, lines: List<&2, String>) -> Stage:
  (board, image) = got
  Stage{window, board, image, labels, assembling, report, quit, pending, input, lines}

def typed(desk: Desk, window: Window, image: Image) -> Stage:
  Desk{board, labels, assembling, report, quit, pending, input, lines} = desk
  Stage{window, board, image, labels, assembling, report, quit, pending, input, lines}

def next_stage(shown: Window & Image & List<&1, Event>, board: Board, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, pending: String, input: String, lines: List<&2, String>, chunk: String) -> Stage:
  (window, image, events) = shown
  gone = Image.drop(image)
  typed(fold(events, pump(chunk, commit(Desk{board, labels, assembling, report, quit, pending, input, lines}))), window, Pix{0})

def with_view(stage: Stage) -> Stage:
  Stage{window, board, image, labels, assembling, report, quit, pending, +input, +lines} = stage
  dropped = Image.drop(image)
  staged(scene(board, lines, prompt(input)), window, labels, assembling, report, quit, pending, input, lines)

@unsafe def loop(stage: Stage) -> IO(Unit):
  match stage:
    case Stage{window, Board{grid, x, y}, image, labels, assembling, report, True{}, pending, input, lines}:
      gone = discard(grid)
      dropped = Image.drop(image)
      quiet = drop_text(input)
      rest = drop_lines(lines)
      do IO<Unit>:
        flush(String.is_empty(report), report)
        Window.close(window)
    case Stage{window, board, image, labels, assembling, +report, False{}, pending, input, lines}:
      do IO<Unit>:
        flush(String.is_empty(report), report)
        shown : Window & Image & List<&1, Event> <- Window.frame(window, image)
        chunk : String <- Poll.stdin(256)
        loop(with_view(next_stage(shown, board, labels, assembling, report, False{}, pending, input, lines, chunk)))

def main() -> IO(Unit):
  do IO<Unit>:
    IO.write("b4 machines. the repl is the panel on the right. click a square, then type.\n")
    IO.write("@@ is that machine's id. ?a reads register a. /all broadcasts. /q quits.\n")
    window : Window <- IO.try(Window, Window.open("b4 machines", 1024, 512))
    loop(staged(scene(Board{grow(7n, 0, 0), 0, 0}, hello(), prompt("")), window, Map.new(&2, U32), False{}, "", False{}, "", "", hello()))
