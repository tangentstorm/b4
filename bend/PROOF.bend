import Base
import ./vm.bend as V
import ./LAWS.bend as L

def L.state_vo():
  {==}

def L.state_ru():
  {==}

def L.state_na():
  {==}

def L.state_ti():
  {==}

def L.add(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.subtract(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.multiply(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.bit_and(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.bit_or(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.bit_xor(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.equal(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.less_than(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.bit_not(mem, ds, cs, ip, control, out, a):
  {==}

def L.duplicate(mem, ds, cs, ip, control, out, a):
  {==}

def L.drop(mem, ds, cs, ip, control, out, a):
  {==}

def L.swap(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.over(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.data_to_control(mem, ds, cs, ip, control, out, a):
  {==}

def L.control_to_data(mem, ds, cs, ip, control, out, a):
  {==}

def L.constant_0(mem, ds, cs, ip, control, out):
  {==}

def L.constant_1(mem, ds, cs, ip, control, out):
  {==}

def L.constant_2(mem, ds, cs, ip, control, out):
  {==}

def L.constant_4294967295(mem, ds, cs, ip, control, out):
  {==}

def L.constant_4(mem, ds, cs, ip, control, out):
  {==}

def L.nop(mem, ds, cs, ip, control, out):
  {==}

def L.debug(mem, ds, cs, ip, control, out):
  {==}

def L.halt(mem, ds, cs, ip, control, out):
  {==}

def L.top_return(mem, ds, cs, ip, control, out):
  {==}

def L.add_underflow(mem, ds, cs, ip, control, out, a):
  {==}

def L.control_underflow(mem, ds, cs, ip, control, out, a):
  {==}

def L.zero_division(mem, ds, cs, ip, control, out, a):
  {==}

def L.zero_modulo(mem, ds, cs, ip, control, out, a):
  {==}

def L.large_shift(mem, ds, cs, ip, control, out, a):
  {==}

def L.negative_large_shift(mem, ds, cs, ip, control, out, a):
  {==}

def L.byte_oob(mem, ds, cs, ip, control, out, a):
  {==}

def L.cell_oob(mem, ds, cs, ip, control, out, a):
  {==}

def L.byte_write_oob(mem, ds, cs, ip, control, out, a):
  {==}

def L.cell_write_oob(mem, ds, cs, ip, control, out, a):
  {==}

def L.unknown_opcode(mem, ds, cs, ip, control, out, a):
  {==}

def L.idle_vo(mem, ds, cs, ip, control, out, reason, at):
  {==}

def L.idle_na(mem, ds, cs, ip, control, out, reason, at):
  {==}

def L.idle_ti(mem, ds, cs, ip, control, out, reason, at):
  {==}

def L.fault_does_not_advance(mem, ds, cs, ip, control, out, reason, at):
  {==}

def L.divide_microcode(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.modulo_microcode(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.shift_microcode(mem, ds, cs, ip, control, out, a, b):
  {==}

def L.read_byte(mem, ds, cs, ip, control, out, a):
  {==}

def L.read_cell(mem, ds, cs, ip, control, out, a):
  {==}

def L.read_signed_byte(mem, ds, cs, ip, control, out, a):
  {==}

def L.write_byte(mem, ds, cs, ip, control, out, a, v):
  {==}

def L.write_cell(mem, ds, cs, ip, control, out, a, v):
  {==}

def L.literal_byte(mem, ds, cs, ip, control, out):
  {==}

def L.literal_cell(mem, ds, cs, ip, control, out):
  {==}

def L.literal_signed_byte(mem, ds, cs, ip, control, out):
  {==}

def L.jump(mem, ds, cs, ip, control, out):
  {==}

def L.call(mem, ds, cs, ip, control, out):
  {==}

def hop_fusion(p: Array<U32> & U32, ds: List<&2, U32>, cs: List<&2, U32>, ip: U32, control: V.Control, out: String) ->
  {V.pair(Array<U32>, U32, V.VM,
    V.pair(Array<U32>, U32, Array<U32> & U32, p, mem => b => (mem, V.jump(U32.add(ip, V.signed_byte(b))))),
    mem => target => V.VM{mem, ds, cs, target, control, out}) ==
   V.pair(Array<U32>, U32, V.VM, p,
    mem => b => V.VM{mem, ds, cs, V.jump(U32.add(ip, V.signed_byte(b))), control, out}) : V.VM}:
  (mem, b) = p
  {==}

def L.hop(mem, ds, cs, ip, control, out):
  +ip = ip
  hop_fusion(Array.get(U32, mem, U32.add(ip, 1)), ds, cs, ip, control, out)

def L.hop_zero(mem, ds, cs, ip, control, out, a):
  {==}

def L.return_address(mem, ds, cs, ip, control, out, a):
  {==}

def L.next(mem, ds, cs, ip, control, out, a):
  {==}

def L.emit(mem, ds, cs, ip, control, out, ch):
  {==}

def L.invoke_register(mem, ds, cs, ip, control, out, r, a):
  {==}

def L.fetch_register(mem, ds, cs, ip, control, out, r, a):
  {==}

def L.store_register(mem, ds, cs, ip, control, out, r, a):
  {==}

def L.stream_register(mem, ds, cs, ip, control, out, r, a):
  {==}

def L.accepted_instruction(mem, ds, cs, ip, control, out, op, stacks, args):
  %Equal.sym(V.Fault, V.stack_check(V.supported(op), V.enough(V.need_data(op), ds), V.enough(V.need_control(op), cs)), V.Clear{}, stacks) : {V.validated(_, op, V.VM{mem, ds, cs, ip, control, out}) == V.microcode(op, V.VM{mem, ds, cs, ip, control, out}) : V.VM}
  %Equal.sym(Array<U32> & V.Fault, V.operands(op, mem, ds, cs, ip), (mem, V.Clear{}), args) : {V.pair(Array<U32>, V.Fault, V.VM, _, mem => reason => V.commit(reason, op, V.VM{mem, ds, cs, ip, control, out})) == V.microcode(op, V.VM{mem, ds, cs, ip, control, out}) : V.VM}
  {==}

def L.atomic_dataunderflow(mem, ds, cs, ip, control, out, op):
  {==}

def L.atomic_controlunderflow(mem, ds, cs, ip, control, out, op):
  {==}

def L.atomic_address(mem, ds, cs, ip, control, out, op):
  {==}

def L.atomic_zerodivisor(mem, ds, cs, ip, control, out, op):
  {==}

def L.atomic_shiftrange(mem, ds, cs, ip, control, out, op):
  {==}

def L.atomic_opcode(mem, ds, cs, ip, control, out, op):
  {==}

def L.atomic_device(mem, ds, cs, ip, control, out, op):
  {==}

def L.advance_vo(mem, ds, cs, ip, control, out, reason, at):
  {==}

def L.advance_ru(mem, ds, cs, ip, control, out, reason, at):
  {==}

def L.advance_ti(mem, ds, cs, ip, control, out, reason, at):
  {==}

def L.zero_quantum(vm):
  {==}

def L.run_idle_vo(mem, ds, cs, ip, control, out, fuel, reason, at):
  match fuel:
    case 0n:
      {==}
    case 1n+p:
      {==}

def L.run_idle_na(mem, ds, cs, ip, control, out, fuel, reason, at):
  match fuel:
    case 0n:
      {==}
    case 1n+p:
      {==}

def L.run_idle_ti(mem, ds, cs, ip, control, out, fuel, reason, at):
  match fuel:
    case 0n:
      {==}
    case 1n+p:
      {==}

def L.fleet_isolation(left, right):
  {==}

def L.valid_divide(mem, ds, cs, ip, control, out, a, b, valid):
  %Equal.sym(Bool, U32.is_ne(b, 0), True{}, valid) : {V.commit(V.check(_, V.ZeroDivisor{}), 131, V.VM{mem, b <> a <> ds, cs, ip, control, out}) == V.VM{mem, V.divide(a, b) <> ds, cs, ip, control, out} : V.VM}
  {==}

def L.valid_modulo(mem, ds, cs, ip, control, out, a, b, valid):
  %Equal.sym(Bool, U32.is_ne(b, 0), True{}, valid) : {V.commit(V.check(_, V.ZeroDivisor{}), 132, V.VM{mem, b <> a <> ds, cs, ip, control, out}) == V.VM{mem, V.remainder(a, b) <> ds, cs, ip, control, out} : V.VM}
  {==}

def L.valid_shift(mem, ds, cs, ip, control, out, a, b, valid):
  %Equal.sym(Bool, U32.is_lt(V.magnitude(b), 32), True{}, valid) : {V.commit(V.check(_, V.ShiftRange{}), 133, V.VM{mem, b <> a <> ds, cs, ip, control, out}) == V.VM{mem, V.shift(a, b) <> ds, cs, ip, control, out} : V.VM}
  {==}

def L.little_endian_store(mem, a, v):
  {==}

def L.little_endian_load(mem, a):
  {==}

def L.division_fault(mem, ds, cs, ip, control, out, a, b, invalid):
  %Equal.sym(Bool, U32.is_ne(b, 0), False{}, invalid) : {V.commit(V.check(_, V.ZeroDivisor{}), 131, V.VM{mem, b <> a <> ds, cs, ip, control, out}) == V.VM{mem, b <> a <> ds, cs, ip, V.Control{V.Na{}, V.ZeroDivisor{}, ip}, out} : V.VM}
  {==}

def L.shift_fault(mem, ds, cs, ip, control, out, a, b, invalid):
  %Equal.sym(Bool, U32.is_lt(V.magnitude(b), 32), False{}, invalid) : {V.commit(V.check(_, V.ShiftRange{}), 133, V.VM{mem, b <> a <> ds, cs, ip, control, out}) == V.VM{mem, b <> a <> ds, cs, ip, V.Control{V.Na{}, V.ShiftRange{}, ip}, out} : V.VM}
  {==}

def L.byte_address_fault(mem, ds, cs, ip, control, out, a, invalid):
  %Equal.sym(Bool, V.address_ok(a, 1), False{}, invalid) : {V.commit(V.check(_, V.Address{}), 146, V.VM{mem, a <> ds, cs, ip, control, out}) == V.VM{mem, a <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}
  {==}

def L.cell_address_fault(mem, ds, cs, ip, control, out, a, invalid):
  %Equal.sym(Bool, V.address_ok(a, 4), False{}, invalid) : {V.commit(V.check(_, V.Address{}), 147, V.VM{mem, a <> ds, cs, ip, control, out}) == V.VM{mem, a <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}
  {==}

def L.byte_store_fault(mem, ds, cs, ip, control, out, a, v, invalid):
  %Equal.sym(Bool, V.address_ok(a, 1), False{}, invalid) : {V.commit(V.check(_, V.Address{}), 148, V.VM{mem, a <> v <> ds, cs, ip, control, out}) == V.VM{mem, a <> v <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}
  {==}

def L.cell_store_fault(mem, ds, cs, ip, control, out, a, v, invalid):
  %Equal.sym(Bool, V.address_ok(a, 4), False{}, invalid) : {V.commit(V.check(_, V.Address{}), 149, V.VM{mem, a <> v <> ds, cs, ip, control, out}) == V.VM{mem, a <> v <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}
  {==}

def L.fetch_fault(mem, ds, cs, ip, control, out, invalid):
  %Equal.sym(Bool, V.code_ok(ip), False{}, invalid) : {V.fetch(_, V.VM{mem, ds, cs, ip, control, out}) == V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}
  {==}

def L.run_successor(vm, n):
  match vm:
    case V.VM{mem, ds, cs, ip, V.Control{V.Ru{}, reason, at}, out}:
      {==}
    case V.VM{mem, ds, cs, ip, V.Control{V.Vo{}, reason, at}, out}:
      Equal.sym(V.VM, V.run(n, V.VM{mem, ds, cs, ip, V.Control{V.Vo{}, reason, at}, out}), V.VM{mem, ds, cs, ip, V.Control{V.Vo{}, reason, at}, out}, L.run_idle_vo(mem, ds, cs, ip, V.Control{V.Vo{}, reason, at}, out, n, reason, at))
    case V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out}:
      Equal.sym(V.VM, V.run(n, V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out}), V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out}, L.run_idle_na(mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out, n, reason, at))
    case V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, out}:
      Equal.sym(V.VM, V.run(n, V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, out}), V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, out}, L.run_idle_ti(mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, out, n, reason, at))

