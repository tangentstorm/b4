# A shell over 16384 B4 machines.
#
# Every turn ticks every machine once. The bang runs that tick on the GPU
# when a device is present. The line you type is applied to the attached
# machine only. /m tx ty (hex) attaches to another one.
import Base
import ./vm.bend as V
import ./text.bend as T
import ./b4i.bend as I

def Poll.stdin(+max: U32) -> IO(String):
  import "./poll_stdin.c"
  import "./poll_stdin.js"

type Node is Type:
  Here{vm: V.VM}
  Away{}
  Pair{left: Node, right: Node}

type Turn is Data:
  L{}
  R{}

type Path is Type:
  End{}
  Go{turn: Turn, rest: Path}

type Desk is Type:
  Desk{fleet: Node, focus: U32, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool}

def fresh() -> V.VM:
  mem = [0 : U32^12n]
  mem = V.write_cell(mem, 124, 256)
  # @A c1 ad !A jm 256. Each lap adds one to register A.
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

def stopped() -> V.VM:
  mem = [0 : U32^12n]
  V.VM{V.write_cell(mem, 124, 256), [], [], 256, V.Control{V.Ti{}, V.Clear{}, 0}, ""}

def sink(vm: V.VM) -> U32:
  V.VM{mem, ds, cs, ip, control, output} = vm
  0

def discard(node: Node) -> U32:
  match node:
    case Here{vm}:
      sink(vm)
    case Away{}:
      0
    case Pair{left, right}:
      a b = discard(left) discard(right)
      U32.add(a, b)

def discard_path(path: Path) -> U32:
  match path:
    case End{}:
      0
    case Go{turn, rest}:
      discard_path(rest)

def bit_at(+index: U32, +shift: Nat) -> Bool:
  U32.is_eq(U32.and(U32.shrn(index, shift), 1), 1)

def turn_of(hi: Bool) -> Turn:
  match hi:
    case True{}:
      R{}
    case False{}:
      L{}

def turns(+n: Nat, +index: U32) -> Path:
  match n:
    case 0n:
      End{}
    case 1n+p:
      Go{turn_of(bit_at(index, p)), turns(p, index)}

def join_left(got: V.VM & Node, right: Node) -> V.VM & Node:
  (vm, left) = got
  (vm, Pair{left, right})

def join_right(got: V.VM & Node, left: Node) -> V.VM & Node:
  (vm, right) = got
  (vm, Pair{left, right})

def drop_walk(path: Path, node: Node) -> V.VM & Node:
  gone = discard(node)
  path_gone = discard_path(path)
  (fresh(), Away{})

def walk(path: Path, node: Node) -> V.VM & Node:
  match path node:
    case End{} Here{vm}:
      (vm, Away{})
    case End{} Away{}:
      (fresh(), Away{})
    case End{} Pair{left, right}:
      drop_walk(End{}, Pair{left, right})
    case Go{L{}, rest} Pair{left, right}:
      join_left(walk(rest, left), right)
    case Go{R{}, rest} Pair{left, right}:
      join_right(walk(rest, right), left)
    case Go{turn, rest} other:
      drop_walk(Go{turn, rest}, other)

def put_left(child: Node, right: Node) -> Node:
  Pair{child, right}

def put_right(left: Node, child: Node) -> Node:
  Pair{left, child}

def drop_put(path: Path, node: Node, vm: V.VM) -> Node:
  gone = discard(node)
  path_gone = discard_path(path)
  Here{vm}

def put(path: Path, node: Node, vm: V.VM) -> Node:
  match path node:
    case End{} Away{}:
      Here{vm}
    case End{} Here{old}:
      gone = sink(old)
      Here{vm}
    case End{} Pair{left, right}:
      drop_put(End{}, Pair{left, right}, vm)
    case Go{L{}, rest} Pair{left, right}:
      put_left(put(rest, left, vm), right)
    case Go{R{}, rest} Pair{left, right}:
      put_right(left, put(rest, right, vm))
    case Go{turn, rest} other:
      drop_put(Go{turn, rest}, other, vm)

def tick_node(node: Node) -> Node:
  match node:
    case Here{vm}:
      Here{V.tick(vm)}
    case Away{}:
      Away{}
    case Pair{left, right}:
      a b = tick_node(left) tick_node(right)
      Pair{a, b}

def span(+n: Nat) -> U32:
  match n:
    case 0n:
      1
    case 1n+p:
      U32.add(span(p), span(p))

def grow(+d: Nat, +index: U32) -> Node:
  match d:
    case 0n:
      Here{fresh()}
    case 1n+p:
      step = span(p)
      a b = grow(p, index) grow(p, U32.add(index, step))
      Pair{a, b}

def depth() -> Nat:
  14n

def blank_labels() -> Map<&2, U32>:
  Map.new(&2, U32)

def prompt(+focus: U32) -> String:
  "m " ++ T.hex_fixed(2n, U32.and(focus, 255), "") ++ " " ++ T.hex_fixed(2n, U32.shrn(focus, 8n), "") ++ ">\n"

def note(desk: Desk, text: String) -> Desk:
  Desk{fleet, +focus, labels, assembling, report, quit} = desk
  Desk{fleet, focus, labels, assembling, report ++ text ++ prompt(focus), quit}

def within_tile(+tx: U32, +ty: U32) -> Bool:
  Bool.and(U32.is_lt(tx, 256), U32.is_lt(ty, 64))

def attach(ok: Bool, +tx: U32, +ty: U32, desk: Desk) -> Desk:
  match ok:
    case True{}:
      Desk{fleet, focus, labels, assembling, report, quit} = desk
      +next = U32.add(U32.mul(ty, 256), tx)
      Desk{fleet, next, labels, assembling, report ++ "attached\n" ++ prompt(next), quit}
    case False{}:
      note(desk, "tile is tx 00..ff, ty 00..3f\n")

def with_ty(+tx: U32, got: Maybe<&2, U32>, desk: Desk) -> Desk:
  match got:
    case None{}:
      note(desk, "usage: /m tx ty\n")
    case Some{+ty}:
      attach(within_tile(tx, ty), tx, ty, desk)

def with_tx(got: Maybe<&2, U32>, ty: String, desk: Desk) -> Desk:
  match got:
    case None{}:
      note(desk, "usage: /m tx ty\n")
    case Some{+tx}:
      with_ty(tx, T.read_hex(ty), desk)

def from_shell(shell: I.Shell, +focus: U32, fleet: Node, prior: String) -> Desk:
  I.Shell{vm, labels, assembling, quit, report} = shell
  Desk{put(turns(depth(), focus), fleet, vm), focus, labels, assembling, prior ++ report ++ prompt(focus), quit}

def edited(got: V.VM & Node, +focus: U32, labels: Map<&2, U32>, assembling: Bool, text: String, prior: String) -> Desk:
  (vm, fleet) = got
  from_shell(I.line(text, I.Shell{vm, labels, assembling, False{}, ""}), focus, fleet, prior)

def delegate(text: String, desk: Desk) -> Desk:
  Desk{fleet, +focus, labels, +assembling, report, quit} = desk
  edited(walk(turns(depth(), focus), fleet), focus, labels, assembling, text, report)

def swapped(got: V.VM & Node, +focus: U32, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool, vm: V.VM, text: String) -> Desk:
  (old, rest) = got
  gone = sink(old)
  note(Desk{put(turns(depth(), focus), rest, vm), focus, labels, assembling, report, quit}, text)

def start(got: V.VM & Node, +focus: U32, labels: Map<&2, U32>, assembling: Bool, report: String, quit: Bool) -> Desk:
  (vm, rest) = got
  note(Desk{put(turns(depth(), focus), rest, V.set_mode(vm, V.Ru{})), focus, labels, assembling, report, quit}, "running\n")

def running(desk: Desk) -> Desk:
  Desk{fleet, +focus, labels, assembling, report, quit} = desk
  start(walk(turns(depth(), focus), fleet), focus, labels, assembling, report, quit)

def cleared(desk: Desk) -> Desk:
  Desk{fleet, +focus, labels, assembling, report, quit} = desk
  swapped(walk(turns(depth(), focus), fleet), focus, labels, assembling, report, quit, stopped(), "cleared\n")

def route(words: List<&2, String>, text: String, desk: Desk) -> Desk:
  match words:
    case "/q" <> Nil{}:
      Desk{fleet, focus, labels, assembling, report, quit} = desk
      Desk{fleet, focus, labels, assembling, report, True{}}
    case "/g" <> Nil{}:
      running(desk)
    case "/C" <> Nil{}:
      cleared(desk)
    case "/m" <> tx <> ty <> Nil{}:
      with_tx(T.read_hex(tx), ty, desk)
    case _:
      delegate(text, desk)

def apply_norm(+text: String, desk: Desk) -> Desk:
  route(String.split(text, ' '), text, desk)

def apply_line(text: String, desk: Desk) -> Desk:
  apply_norm(T.normalize(text), desk)

def pump(chunk: String, desk: Desk, pending: String) -> Desk & String:
  match chunk:
    case SNil{}:
      (desk, pending)
    case SCon{Chr{4}, SNil{}}:
      Desk{fleet, focus, labels, assembling, report, quit} = desk
      (Desk{fleet, focus, labels, assembling, report, True{}}, pending)
    case SCon{Chr{10}, tail}:
      pump(tail, apply_line(String.reverse(pending), desk), "")
    case SCon{c, tail}:
      pump(tail, desk, SCon{c, pending})

def flush(empty: Bool, text: String) -> IO(Unit):
  match empty:
    case True{}:
      do IO<Unit>:
        return Unit{}
    case False{}:
      IO.write(text)

@unsafe def loop(got: Desk & String) -> IO(Unit):
  (desk, pending) = got
  Desk{fleet, focus, labels, assembling, +report, quit} = desk
  match quit:
    case True{}:
      flush(String.is_empty(report), report)
    case False{}:
      fleet2 = tick_node!(fleet)
      do IO<Unit>:
        chunk : String <- Poll.stdin(256)
        flush(String.is_empty(report), report)
        loop(pump(chunk, Desk{fleet2, focus, labels, assembling, "", False{}}, pending))

def main() -> IO(Unit):
  do IO<Unit>:
    IO.write("booting 16384 machines\n")
    fleet : Node = grow(depth(), 0)
    IO.write("16384 machines ticking. attached to 00 00.\n")
    IO.write("/m tx ty attaches (hex). /g runs the attached machine. /t stops it. /q quits.\n")
    IO.write(prompt(0))
    loop((Desk{fleet, 0, blank_labels(), False{}, "", False{}}, ""))
