# Minimal b4i console for the shared VM acceptance tests.
import Base
import ./vm.bend as V
import ./text.bend as T

type Input is Data:
  Read{}
  Chunk{text: String}

type Shell is Type:
  Shell{vm: V.VM, labels: Map<&2, U32>, assembling: Bool, quit: Bool, report: String}

def boot() -> Shell:
  Shell{V.boot(), Map.new(&2, U32), False{}, False{}, ""}

def emit(vm: V.VM, value: U32) -> V.VM:
  V.state(V.VM, vm, mem => ds => cs => ip => control => output => (
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, 124), mem => +here => (
      mem[here] <- V.low_byte(value)
      mem = V.write_cell(mem, 124, U32.add(here, 1))
      V.VM{mem, ds, cs, ip, control, output}
    ))
  ))

def set_here(vm: V.VM, addr: U32) -> V.VM:
  V.VM{mem, ds, cs, ip, control, output} = vm
  V.VM{V.write_cell(mem, 124, addr), ds, cs, ip, control, output}

def resume(vm: V.VM) -> V.VM:
  V.VM{mem, ds, cs, ip, control, output} = vm
  V.run(1000000n, V.VM{mem, ds, cs, ip, V.Control{V.Ru{}, V.Clear{}, 0}, output})

def invoke_finish(vm: V.VM, saved: U32, depth: Nat) -> V.VM:
  match vm:
    case V.VM{mem, ds, +cs, ip, V.Control{V.Ti{}, reason, at}, output}:
      V.VM{mem, ds, cs, V.pick(Nat.is_eq(List.length(&2, U32, cs), depth), saved, ip), V.Control{V.Ti{}, reason, at}, output}
    case V.VM{mem, ds, cs, ip, control, output}:
      V.VM{mem, ds, cs, ip, control, output}

def invoke_valid(vm: V.VM, addr: U32) -> V.VM:
  V.VM{mem, ds, +cs, ip, control, output} = vm
  depth = List.length(&2, U32, cs)
  V.state(V.VM, V.run(1000000n, V.VM{mem, ds, 0 <> cs, addr, V.Control{V.Ru{}, V.Clear{}, 0}, output}),
    mem => ds => cs => end => control => output => invoke_finish(V.VM{mem, ds, cs, end, control, output}, ip, depth))

def invoke_checked(ok: Bool, vm: V.VM, addr: U32) -> V.VM:
  match ok:
    case True{}:
      invoke_valid(vm, addr)
    case False{}:
      V.fault(vm, V.Address{})

def invoke(vm: V.VM, +addr: U32) -> V.VM:
  invoke_checked(V.code_ok(addr), vm, addr)

def invoke_register(vm: V.VM, r: U32) -> V.VM:
  V.state(V.VM, vm, mem => ds => cs => ip => control => output => (
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, U32.mul(r, 4)), mem => addr => (
      invoke(V.VM{mem, ds, cs, ip, control, output}, addr)
    ))
  ))

def dump(n: Nat, mem: Array<U32>, +addr: U32) -> Array<U32> & String:
  match n:
    case 0n:
      (mem, "")
    case 1n+p:
      V.pair(Array<U32>, U32, Array<U32> & String, mem[addr], mem => b => (
        V.pair(Array<U32>, String, Array<U32> & String, dump(p, mem, U32.add(addr, 1)), mem => +rest => (
          (mem, T.byte(b) ++ Bool.pick(String, String.is_empty(rest), "", " " ++ rest))
        ))
      ))

def query_address(addr: Maybe<&2, U32>, vm: V.VM) -> V.VM & String:
  match addr:
    case None{}:
      (vm, "unknown query\n")
    case Some{a}:
      V.state(V.VM & String, vm, mem => ds => cs => ip => control => output => (
        V.pair(Array<U32>, String, V.VM & String, dump(16n, mem, a), mem => text => (
          (V.VM{mem, ds, cs, ip, control, output}, text ++ "\n")
        ))
      ))

def query_register(reg: Maybe<&2, U32>, name: String, vm: V.VM) -> V.VM & String:
  match reg:
    case None{}:
      query_address(T.read_hex(name), vm)
    case Some{r}:
      V.state(V.VM & String, vm, mem => ds => cs => ip => control => output => (
        V.pair(Array<U32>, U32, V.VM & String, V.read_cell(mem, U32.mul(r, 4)), mem => value => (
          (V.VM{mem, ds, cs, ip, control, output}, T.hex_fixed(8n, value, "") ++ "\n")
        ))
      ))

def fault_text(reason: V.Fault) -> String:
  match reason:
    case V.Clear{}:
      "clear"
    case V.DataUnderflow{}:
      "data-underflow"
    case V.ControlUnderflow{}:
      "control-underflow"
    case V.Address{}:
      "invalid-address"
    case V.ZeroDivisor{}:
      "division-by-zero"
    case V.ShiftRange{}:
      "shift-out-of-range"
    case V.Opcode{}:
      "unsupported-opcode"
    case V.Device{}:
      "unsupported-device"

def control_text(control: V.Control) -> String:
  match control:
    case V.Control{V.Vo{}, reason, at}:
      "vo"
    case V.Control{V.Ru{}, reason, at}:
      "ru"
    case V.Control{V.Na{}, reason, at}:
      "na " ++ fault_text(reason) ++ " at " ++ T.hex(at)
    case V.Control{V.Ti{}, reason, at}:
      "ti"

def query(name: String, vm: V.VM) -> V.VM & String:
  match name:
    case "d":
      V.state(V.VM & String, vm, mem => ds => cs => ip => control => output => (
        V.pair(List<&2, U32>, String, V.VM & String, T.stack(ds), ds => text => (
          (V.VM{mem, ds, cs, ip, control, output}, "ds: [" ++ text ++ "]\n")
        ))
      ))
    case "c":
      V.state(V.VM & String, vm, mem => ds => cs => ip => control => output => (
        V.pair(List<&2, U32>, String, V.VM & String, T.stack(cs), cs => text => (
          (V.VM{mem, ds, cs, ip, control, output}, "cs: [" ++ text ++ "]\n")
        ))
      ))
    case "s":
      V.VM{mem, ds, cs, ip, +control, output} = vm
      (V.VM{mem, ds, cs, ip, control, output}, "state: " ++ control_text(control) ++ "\n")
    case "i":
      V.VM{mem, ds, cs, +ip, control, output} = vm
      (V.VM{mem, ds, cs, ip, control, output}, "ip: " ++ T.hex(ip) ++ "\n")
    case +other:
      query_register(T.register(other), other, vm)

def define_named(name: String, reg: Maybe<&2, U32>, vm: V.VM,
                 labels: Map<&2, U32>, quit: Bool, report: String) -> Shell:
  match reg:
    case Some{r}:
      V.state(Shell, vm, mem => ds => cs => ip => control => output => (
        V.pair(Array<U32>, U32, Shell, V.read_cell(mem, 124), mem => here => (
          mem = V.write_cell(mem, U32.mul(r, 4), here)
          Shell{V.VM{mem, ds, cs, ip, control, output}, labels, True{}, quit, report}
        ))
      ))
    case None{}:
      V.state(Shell, vm, mem => ds => cs => ip => control => output => (
        V.pair(Array<U32>, U32, Shell, V.read_cell(mem, 124), mem => here => (
          Shell{V.VM{mem, ds, cs, ip, control, output}, Map.set(&2, U32, labels, name, here), True{}, quit, report}
        ))
      ))

def define_number(number: Maybe<&2, U32>, +name: String, vm: V.VM,
                  labels: Map<&2, U32>, quit: Bool, report: String) -> Shell:
  match number:
    case Some{a}:
      Shell{set_here(vm, a), labels, True{}, quit, report}
    case None{}:
      define_named(name, T.register(name), vm, labels, quit, report)

# A single capital letter always defines a register, including A through F.

def define(reg: Maybe<&2, U32>, +name: String, vm: V.VM,
           labels: Map<&2, U32>, quit: Bool, report: String) -> Shell:
  match reg:
    case Some{r}:
      define_named(name, Some{r}, vm, labels, quit, report)
    case None{}:
      define_number(T.read_hex(name), name, vm, labels, quit, report)

def number(assembling: Bool, vm: V.VM, value: U32) -> V.VM:
  match assembling:
    case True{}:
      emit(vm, value)
    case False{}:
      V.push(vm, value)

def calculator(vm: V.VM, op: U32) -> V.VM:
  match vm:
    case V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, output}:
      V.execute(op, V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, output})
    case V.VM{mem, ds, cs, ip, control, output}:
      V.VM{mem, ds, cs, ip, control, output}

def operation(assembling: Bool, vm: V.VM, op: U32) -> V.VM:
  match assembling:
    case True{}:
      emit(vm, op)
    case False{}:
      calculator(vm, op)

def invoke_or_emit(assembling: Bool, vm: V.VM, r: U32) -> V.VM:
  match assembling:
    case True{}:
      emit(vm, r)
    case False{}:
      invoke_register(vm, r)

def reg_operation(prefix: U32, r: U32, assembling: Bool, vm: V.VM) -> V.VM:
  match prefix:
    case 96:
      number(assembling, vm, U32.mul(r, 4))
    case 64:
      operation(assembling, vm, U32.add(32, r))
    case 33:
      operation(assembling, vm, U32.add(64, r))
    case 43:
      operation(assembling, vm, U32.add(96, r))
    case 94:
      invoke_or_emit(assembling, vm, r)
    case _:
      vm

def emit_call(vm: V.VM, +addr: U32) -> V.VM:
  vm = emit(vm, 157)
  vm = emit(vm, addr)
  vm = emit(vm, U32.shrn(addr, 8n))
  vm = emit(vm, U32.shrn(addr, 16n))
  emit(vm, U32.shrn(addr, 24n))

def label_call(assembling: Bool, vm: V.VM, addr: U32) -> V.VM:
  match assembling:
    case True{}:
      emit_call(vm, addr)
    case False{}:
      invoke(vm, addr)

def found_label(found: Bool, addr: U32, name: String, vm: V.VM, labels: Map<&2, U32>,
                +assembling: Bool, quit: Bool, report: String) -> Shell:
  match found:
    case True{}:
      Shell{label_call(assembling, vm, addr), labels, assembling, quit, report}
    case False{}:
      Shell{vm, labels, assembling, quit, report ++ "unknown token: " ++ name ++ "\n"}

def literal(n: Maybe<&2, U32>, +name: String, vm: V.VM, labels: Map<&2, U32>,
            +assembling: Bool, quit: Bool, report: String) -> Shell:
  match n:
    case Some{x}:
      Shell{number(assembling, vm, x), labels, assembling, quit, report}
    case None{}:
      V.pair(Map<&2, U32>, U32, Shell, Map.get(U32, 0, labels, name), labels => +addr => (
        found_label(U32.is_ne(addr, 0), addr, name, vm, labels, assembling, quit, report)
      ))

def ordinary_op(known: Bool, op: U32, +name: String, vm: V.VM, labels: Map<&2, U32>,
                +assembling: Bool, quit: Bool, report: String) -> Shell:
  match known:
    case True{}:
      Shell{operation(assembling, vm, op), labels, assembling, quit, report}
    case False{}:
      literal(T.read_hex(name), name, vm, labels, assembling, quit, report)

def ordinary(+name: String, vm: V.VM, labels: Map<&2, U32>,
             assembling: Bool, quit: Bool, report: String) -> Shell:
  +op = T.opcode(name)
  ordinary_op(U32.is_lt(op, 256), op, name, vm, labels, assembling, quit, report)

def emit_literal(vm: V.VM, +addr: U32) -> V.VM:
  vm = emit(vm, 151)
  vm = emit(vm, addr)
  vm = emit(vm, U32.shrn(addr, 8n))
  vm = emit(vm, U32.shrn(addr, 16n))
  emit(vm, U32.shrn(addr, 24n))

def address_literal(assembling: Bool, vm: V.VM, addr: U32) -> V.VM:
  match assembling:
    case True{}:
      emit_literal(vm, addr)
    case False{}:
      V.push(vm, addr)

def access_label(prefix: U32, +assembling: Bool, vm: V.VM, addr: U32) -> V.VM:
  match prefix:
    case 64:
      operation(assembling, address_literal(assembling, vm, addr), 147)
    case 33:
      operation(assembling, address_literal(assembling, vm, addr), 149)
    case _:
      address_literal(assembling, vm, addr)

def label_reference(found: Bool, prefix: U32, addr: U32, name: String, vm: V.VM,
                    labels: Map<&2, U32>, +assembling: Bool, quit: Bool, report: String) -> Shell:
  match found:
    case True{}:
      Shell{access_label(prefix, assembling, vm, addr), labels, assembling, quit, report}
    case False{}:
      Shell{vm, labels, assembling, quit, report ++ "unknown label: " ++ name ++ "\n"}

def prefixed_known(is_prefix: Bool, prefix: U32, +name: String, vm: V.VM,
                   labels: Map<&2, U32>, assembling: Bool, quit: Bool, report: String) -> Shell:
  match is_prefix:
    case False{}:
      ordinary(name, vm, labels, assembling, quit, report)
    case True{}:
      V.pair(Map<&2, U32>, U32, Shell, Map.get(U32, 0, labels, String.drop(name, 1n)), labels => +addr => (
        label_reference(U32.is_ne(addr, 0), prefix, addr, name, vm, labels, assembling, quit, report)
      ))

def prefixed(+prefix: U32, name: String, vm: V.VM, labels: Map<&2, U32>,
             assembling: Bool, quit: Bool, report: String) -> Shell:
  prefixed_known(Bool.or(U32.is_eq(prefix, 64), Bool.or(U32.is_eq(prefix, 33), U32.is_eq(prefix, 96))),
    prefix, name, vm, labels, assembling, quit, report)

def register_token(reg: Maybe<&2, U32>, prefix: U32, name: String, vm: V.VM, labels: Map<&2, U32>,
                   +assembling: Bool, quit: Bool, report: String) -> Shell:
  match reg:
    case None{}:
      prefixed(prefix, name, vm, labels, assembling, quit, report)
    case Some{r}:
      Shell{reg_operation(prefix, r, assembling, vm), labels, assembling, quit, report}

def reg_prefix(prefix: U32, tail: String) -> Maybe<&2, U32>:
  match prefix:
    case 96:
      T.register(tail)
    case 64:
      T.register(tail)
    case 33:
      T.register(tail)
    case 43:
      T.register(tail)
    case 94:
      T.register(tail)
    case _:
      None{}

def single_step(vm: V.VM) -> V.VM:
  V.step(V.set_mode(vm, V.Ti{}))

def token(word: String, shell: Shell) -> Shell:
  match word:
    case "":
      shell
    case "/q":
      Shell{vm, labels, assembling, quit, report} = shell
      Shell{vm, labels, assembling, True{}, report}
    case "/C":
      boot()
    case "/s":
      Shell{vm, labels, assembling, quit, report} = shell
      Shell{single_step(vm), labels, False{}, quit, report}
    case "/t":
      Shell{vm, labels, assembling, quit, report} = shell
      Shell{V.set_mode(vm, V.Ti{}), labels, False{}, quit, report}
    case "/o":
      Shell{vm, labels, assembling, quit, report} = shell
      Shell{V.set_mode(vm, V.Vo{}), labels, False{}, quit, report}
    case "/g":
      Shell{vm, labels, assembling, quit, report} = shell
      Shell{resume(vm), labels, False{}, quit, report}
    case SCon{Chr{63}, name}:
      Shell{vm, labels, assembling, quit, report} = shell
      V.pair(V.VM, String, Shell, query(name, vm), vm => text => (
        Shell{vm, labels, False{}, quit, report ++ text}
      ))
    case ":":
      Shell{vm, labels, assembling, quit, report} = shell
      Shell{vm, labels, True{}, quit, report}
    case SCon{Chr{58}, +name}:
      Shell{vm, labels, assembling, quit, report} = shell
      define(T.register(name), name, vm, labels, quit, report)
    case "'":
      Shell{vm, labels, +assembling, quit, report} = shell
      Shell{number(assembling, vm, 32), labels, assembling, quit, report}
    case SCon{Chr{39}, SCon{Chr{ch}, SNil{}}}:
      Shell{vm, labels, +assembling, quit, report} = shell
      Shell{number(assembling, vm, ch), labels, assembling, quit, report}
    case SCon{Chr{+prefix}, +tail}:
      Shell{vm, labels, assembling, quit, report} = shell
      register_token(reg_prefix(prefix, tail), prefix, SCon{Chr{prefix}, tail}, vm, labels, assembling, quit, report)

def tokens(words: List<&2, String>, shell: Shell) -> Shell:
  match words shell:
    case Nil{} shell:
      shell
    case h <> t Shell{vm, labels, assembling, True{}, report}:
      Shell{vm, labels, assembling, True{}, report}
    case h <> t Shell{vm, labels, assembling, False{}, report}:
      tokens(t, token(h, Shell{vm, labels, assembling, False{}, report}))

def line(text: String, shell: Shell) -> Shell:
  Shell{vm, labels, assembling, quit, report} = shell
  tokens(String.split(T.normalize(text), ' '), Shell{vm, labels, False{}, quit, report})

def flush(shell: Shell) -> IO(Shell):
  Shell{vm, labels, assembling, quit, report} = shell
  V.state(IO(Shell), vm, mem => ds => cs => ip => control => +output => (
    do IO<Shell>:
      IO.write(report ++ Bool.pick(String, String.is_empty(output), "", output ++ "\n"))
      return Shell{V.VM{mem, ds, cs, ip, control, ""}, labels, assembling, quit, ""}
  ))

# Only the external input loop is unsafe: its termination depends on EOF.
# Bytecode execution itself always has a checked, finite fuel argument.

@unsafe def loop(input: Input, file: File, shell: Shell, pending: String) -> IO(Unit):
  match input shell:
    case Read{} Shell{vm, labels, assembling, True{}, report}:
      File.close(file)
    case Read{} shell:
      do IO<Unit>:
        got : File & Result<&1, &1, U32 & String, String> <- File.read(file, 1)
        V.pair(File, Result<&1, &1, U32 & String, String>, IO(Unit), got, file => result => (
          do IO<Unit>:
            text : String <- IO.pass(String, result)
            loop(Chunk{text}, file, shell, pending)
        ))
    case Chunk{SNil{}} shell:
      do IO<Unit>:
        final : Shell <- flush(line(String.reverse(pending), shell))
        File.close(file)
    case Chunk{SCon{Chr{10}, tail}} shell:
      do IO<Unit>:
        next : Shell <- flush(line(String.reverse(pending), shell))
        loop(Read{}, file, next, "")
    case Chunk{SCon{c, tail}} shell:
      loop(Read{}, file, shell, SCon{c, pending})

def main() -> IO(Unit):
  do IO<Unit>:
    file : File <- IO.try(File, File.open("/dev/stdin", "r"))
    loop(Read{}, file, boot(), "")


# The sole unsafe recursion is the external input loop, bounded by EOF or /q.

