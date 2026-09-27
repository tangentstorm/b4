# Agreed B4 contracts. Proofs live separately in PROOF.bend.
# Cells are U32 bit patterns; list heads represent the tops of the stacks.
import Base
import ./vm.bend as V

law state_vo:
  {V.mode_code(V.Vo{}) == 0 : U32}
law state_ru:
  {V.mode_code(V.Ru{}) == 1 : U32}
law state_na:
  {V.mode_code(V.Na{}) == 2 : U32}
law state_ti:
  {V.mode_code(V.Ti{}) == 3 : U32}

law add:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(128,
    V.VM{mem, b <> a <> ds,        cs, ip, control, out}) ==
    V.VM{mem, U32.add(a, b) <> ds, cs, ip, control, out} : V.VM}

law subtract:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(129,
    V.VM{mem, b <> a <> ds,        cs, ip, control, out}) ==
    V.VM{mem, U32.sub(a, b) <> ds, cs, ip, control, out} : V.VM}

law multiply:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(130,
    V.VM{mem, b <> a <> ds,        cs, ip, control, out}) ==
    V.VM{mem, U32.mul(a, b) <> ds, cs, ip, control, out} : V.VM}

law bit_and:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(134,
    V.VM{mem, b <> a <> ds,        cs, ip, control, out}) ==
    V.VM{mem, U32.and(a, b) <> ds, cs, ip, control, out} : V.VM}

law bit_or:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(135,
    V.VM{mem, b <> a <> ds,       cs, ip, control, out}) ==
    V.VM{mem, U32.or(a, b) <> ds, cs, ip, control, out} : V.VM}

law bit_xor:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(136,
    V.VM{mem, b <> a <> ds,        cs, ip, control, out}) ==
    V.VM{mem, U32.xor(a, b) <> ds, cs, ip, control, out} : V.VM}

law equal:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(138,
    V.VM{mem, b <> a <> ds,                   cs, ip, control, out}) ==
    V.VM{mem, V.truth(U32.is_eq(a, b)) <> ds, cs, ip, control, out} : V.VM}

law less_than:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(139,
    V.VM{mem, b <> a <> ds,                     cs, ip, control, out}) ==
    V.VM{mem, V.truth(V.signed_lt(a, b)) <> ds, cs, ip, control, out} : V.VM}

law bit_not:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(137,
    V.VM{mem, a <> ds,          cs, ip, control, out}) ==
    V.VM{mem, U32.not(a) <> ds, cs, ip, control, out} : V.VM}

law duplicate:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(140,
    V.VM{mem, a <> ds,      cs, ip, control, out}) ==
    V.VM{mem, a <> a <> ds, cs, ip, control, out} : V.VM}

law drop:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(143,
    V.VM{mem, a <> ds, cs, ip, control, out}) ==
    V.VM{mem, ds,      cs, ip, control, out} : V.VM}

law swap:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(141,
    V.VM{mem, b <> a <> ds, cs, ip, control, out}) ==
    V.VM{mem, a <> b <> ds, cs, ip, control, out} : V.VM}

law over:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.execute(142,
    V.VM{mem, b <> a <> ds,      cs, ip, control, out}) ==
    V.VM{mem, a <> b <> a <> ds, cs, ip, control, out} : V.VM}

law data_to_control:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(144,
    V.VM{mem, a <> ds, cs,      ip, control, out}) ==
    V.VM{mem, ds,      a <> cs, ip, control, out} : V.VM}

law control_to_data:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(145,
    V.VM{mem, ds,      a <> cs, ip, control, out}) ==
    V.VM{mem, a <> ds, cs,      ip, control, out} : V.VM}

law constant_0:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(192,
    V.VM{mem, ds,      cs, ip, control, out}) ==
    V.VM{mem, 0 <> ds, cs, ip, control, out} : V.VM}

law constant_1:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(193,
    V.VM{mem, ds,      cs, ip, control, out}) ==
    V.VM{mem, 1 <> ds, cs, ip, control, out} : V.VM}

law constant_2:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(246,
    V.VM{mem, ds,      cs, ip, control, out}) ==
    V.VM{mem, 2 <> ds, cs, ip, control, out} : V.VM}

law constant_4294967295:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(247,
    V.VM{mem, ds,               cs, ip, control, out}) ==
    V.VM{mem, 4294967295 <> ds, cs, ip, control, out} : V.VM}

law constant_4:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(248,
    V.VM{mem, ds,      cs, ip, control, out}) ==
    V.VM{mem, 4 <> ds, cs, ip, control, out} : V.VM}

law nop:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(0,
    V.VM{mem, ds, cs, ip, control, out}) ==
    V.VM{mem, ds, cs, ip, control, out} : V.VM}

law debug:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(254,
    V.VM{mem, ds, cs, ip, control,                         out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, V.Clear{}, 0}, out} : V.VM}

law halt:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(255,
    V.VM{mem, ds, cs, ip, control,                         out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, V.Clear{}, 0}, out} : V.VM}

law top_return:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.execute(158,
    V.VM{mem, ds, 0 <> cs, ip, control,                         out}) ==
    V.VM{mem, ds, cs,      ip, V.Control{V.Ti{}, V.Clear{}, 0}, out} : V.VM}

law add_underflow:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(128,
    V.VM{mem, a <> [], cs, ip, control,                                  out}) ==
    V.VM{mem, a <> [], cs, ip, V.Control{V.Na{}, V.DataUnderflow{}, ip}, out} : V.VM}

law control_underflow:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(145,
    V.VM{mem, ds, [], ip, control,                                     out}) ==
    V.VM{mem, ds, [], ip, V.Control{V.Na{}, V.ControlUnderflow{}, ip}, out} : V.VM}

law zero_division:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(131,
    V.VM{mem, 0 <> a <> ds, cs, ip, control,                                out}) ==
    V.VM{mem, 0 <> a <> ds, cs, ip, V.Control{V.Na{}, V.ZeroDivisor{}, ip}, out} : V.VM}

law zero_modulo:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(132,
    V.VM{mem, 0 <> a <> ds, cs, ip, control,                                out}) ==
    V.VM{mem, 0 <> a <> ds, cs, ip, V.Control{V.Na{}, V.ZeroDivisor{}, ip}, out} : V.VM}

law large_shift:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(133,
    V.VM{mem, 32 <> a <> ds, cs, ip, control,                               out}) ==
    V.VM{mem, 32 <> a <> ds, cs, ip, V.Control{V.Na{}, V.ShiftRange{}, ip}, out} : V.VM}

law negative_large_shift:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(133,
    V.VM{mem, 4294967264 <> a <> ds, cs, ip, control,                               out}) ==
    V.VM{mem, 4294967264 <> a <> ds, cs, ip, V.Control{V.Na{}, V.ShiftRange{}, ip}, out} : V.VM}

law byte_oob:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(146,
    V.VM{mem, 65536 <> ds, cs, ip, control,                            out}) ==
    V.VM{mem, 65536 <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law cell_oob:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(147,
    V.VM{mem, 65533 <> ds, cs, ip, control,                            out}) ==
    V.VM{mem, 65533 <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law byte_write_oob:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(148,
    V.VM{mem, 65536 <> a <> ds, cs, ip, control,                            out}) ==
    V.VM{mem, 65536 <> a <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law cell_write_oob:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(149,
    V.VM{mem, 65533 <> a <> ds, cs, ip, control,                            out}) ==
    V.VM{mem, 65533 <> a <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law unknown_opcode:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.execute(191,
    V.VM{mem, ds, cs, ip, control,                           out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.Opcode{}, ip}, out} : V.VM}

law idle_vo:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for reason: V.Fault
  for at: U32
  {V.tick(
    V.VM{mem, ds, cs, ip, V.Control{V.Vo{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Vo{}, reason, at}, out} : V.VM}

law idle_na:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for reason: V.Fault
  for at: U32
  {V.tick(
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out} : V.VM}

law idle_ti:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for reason: V.Fault
  for at: U32
  {V.tick(
    V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, out} : V.VM}

law fault_does_not_advance:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for reason: V.Fault
  for at: U32
  {V.advance(
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out} : V.VM}

law divide_microcode:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.microcode(131,
    V.VM{mem, b <> a <> ds,         cs, ip, control, out}) ==
    V.VM{mem, V.divide(a, b) <> ds, cs, ip, control, out} : V.VM}

law modulo_microcode:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.microcode(132,
    V.VM{mem, b <> a <> ds,            cs, ip, control, out}) ==
    V.VM{mem, V.remainder(a, b) <> ds, cs, ip, control, out} : V.VM}

law shift_microcode:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  {V.microcode(133,
    V.VM{mem, b <> a <> ds,        cs, ip, control, out}) ==
    V.VM{mem, V.shift(a, b) <> ds, cs, ip, control, out} : V.VM}

law read_byte:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.microcode(146,
    V.VM{mem, a <> ds, cs, ip, control, out}) ==
    V.pair(Array<U32>, U32, V.VM, mem[a], mem => v =>
    V.VM{mem, v <> ds, cs, ip, control, out}) : V.VM}

law read_cell:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.microcode(147,
    V.VM{mem, a <> ds, cs, ip, control, out}) ==
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, a), mem => v =>
    V.VM{mem, v <> ds, cs, ip, control, out}) : V.VM}

law read_signed_byte:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.microcode(152,
    V.VM{mem, a <> ds,                cs, ip, control, out}) ==
    V.pair(Array<U32>, U32, V.VM, mem[a], mem => v =>
    V.VM{mem, V.signed_byte(v) <> ds, cs, ip, control, out}) : V.VM}

law write_byte:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for v: U32
  {V.microcode(148,
    V.VM{mem,                                     a <> v <> ds, cs, ip, control, out}) ==
    V.VM{Array.set(U32, mem, a, U32.and(v, 255)), ds,           cs, ip, control, out} : V.VM}

law write_cell:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for v: U32
  {V.microcode(149,
    V.VM{mem,                     a <> v <> ds, cs, ip, control, out}) ==
    V.VM{V.write_cell(mem, a, v), ds,           cs, ip, control, out} : V.VM}

law literal_byte:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.microcode(150,
    V.VM{mem, ds,      cs, ip,             control, out}) ==
    V.pair(Array<U32>, U32, V.VM, Array.get(U32, mem, U32.add(ip, 1)), mem => v =>
    V.VM{mem, v <> ds, cs, U32.add(ip, 1), control, out}) : V.VM}

law literal_cell:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.microcode(151,
    V.VM{mem, ds,      cs, ip,             control, out}) ==
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, U32.add(ip, 1)), mem => v =>
    V.VM{mem, v <> ds, cs, U32.add(ip, 4), control, out}) : V.VM}

law literal_signed_byte:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.microcode(153,
    V.VM{mem, ds,                     cs, ip,             control, out}) ==
    V.pair(Array<U32>, U32, V.VM, Array.get(U32, mem, U32.add(ip, 1)), mem => v =>
    V.VM{mem, V.signed_byte(v) <> ds, cs, U32.add(ip, 1), control, out}) : V.VM}

law jump:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.microcode(154,
    V.VM{mem, ds, cs, ip,           control, out}) ==
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, U32.add(ip, 1)), mem => addr =>
    V.VM{mem, ds, cs, V.jump(addr), control, out}) : V.VM}

law call:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.microcode(157,
    V.VM{mem, ds, cs,                   ip,           control, out}) ==
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, U32.add(ip, 1)), mem => addr =>
    V.VM{mem, ds, U32.add(ip, 5) <> cs, V.jump(addr), control, out}) : V.VM}

law hop:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  {V.microcode(155,
    V.VM{mem, ds, cs, ip,                                         control, out}) ==
    V.pair(Array<U32>, U32, V.VM, Array.get(U32, mem, U32.add(ip, 1)), mem => offset =>
    V.VM{mem, ds, cs, V.jump(U32.add(ip, V.signed_byte(offset))), control, out}) : V.VM}

law hop_zero:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.microcode(156,
    V.VM{mem, a <> ds, cs, ip,   control, out}) ==
    V.pair(Array<U32>, U32, V.VM, V.conditional_hop(U32.is_zero(a), mem, ip), mem => addr =>
    V.VM{mem, ds,      cs, addr, control, out}) : V.VM}

law return_address:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.microcode(158,
    V.VM{mem, ds, a <> cs, ip,                                        control,                                   out}) ==
    V.VM{mem, ds, cs,      V.pick(U32.is_zero(a), ip, U32.sub(a, 1)), V.return_control(U32.is_zero(a), control), out} : V.VM}

law next:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  {V.microcode(159,
    V.VM{mem, ds, a <> cs, ip, control, out}) ==
    V.next_jump(U32.is_zero(V.next_counter(a)), mem, ds, cs, V.next_counter(a), ip, control, out) : V.VM}

law emit:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for ch: U32
  {V.execute(253,
    V.VM{mem, 101 <> ch <> ds, cs, ip, control, out}) ==
    V.VM{mem, ds,              cs, ip, control, out ++ SCon{Chr{ch}, SNil{}}} : V.VM}

law invoke_register:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for r: U32
  for a: U32
  {V.register_op(0, r,
    V.VM{mem, ds, cs,                   ip,           control, out}) ==
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, r), mem => addr =>
    V.VM{mem, ds, U32.add(ip, 1) <> cs, V.jump(addr), control, out}) : V.VM}

law fetch_register:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for r: U32
  for a: U32
  {V.register_op(1, r,
    V.VM{mem, ds,          cs, ip, control, out}) ==
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, r), mem => value =>
    V.VM{mem, value <> ds, cs, ip, control, out}) : V.VM}

law store_register:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for r: U32
  for a: U32
  {V.register_op(2, r,
    V.VM{mem,                     a <> ds, cs, ip, control, out}) ==
    V.VM{V.write_cell(mem, r, a), ds,      cs, ip, control, out} : V.VM}

law stream_register:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for r: U32
  for a: U32
  {V.register_op(3, r,
    V.VM{mem,                                     a <> ds,     cs, ip, control, out}) ==
    V.pair(Array<U32>, U32, V.VM, V.read_cell(mem, r), mem => +value =>
    V.VM{V.write_cell(mem, r, U32.add(value, a)), value <> ds, cs, ip, control, out}) : V.VM}

law accepted_instruction:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for op: U32
  for stacks: {V.stack_check(V.supported(op), V.enough(V.need_data(op), ds), V.enough(V.need_control(op), cs)) == V.Clear{} : V.Fault}
  for args: {V.operands(op, mem, ds, cs, ip) == (mem, V.Clear{}) : Array<U32> & V.Fault}
  {V.execute(op,
    V.VM{mem, ds, cs, ip, control, out}) ==
    V.microcode(op,
    V.VM{mem, ds, cs, ip, control, out}) : V.VM}

law atomic_dataunderflow:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for op: U32
  {V.commit(V.DataUnderflow{}, op,
    V.VM{mem, ds, cs, ip, control,                                  out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.DataUnderflow{}, ip}, out} : V.VM}

law atomic_controlunderflow:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for op: U32
  {V.commit(V.ControlUnderflow{}, op,
    V.VM{mem, ds, cs, ip, control,                                     out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.ControlUnderflow{}, ip}, out} : V.VM}

law atomic_address:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for op: U32
  {V.commit(V.Address{}, op,
    V.VM{mem, ds, cs, ip, control,                            out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law atomic_zerodivisor:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for op: U32
  {V.commit(V.ZeroDivisor{}, op,
    V.VM{mem, ds, cs, ip, control,                                out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.ZeroDivisor{}, ip}, out} : V.VM}

law atomic_shiftrange:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for op: U32
  {V.commit(V.ShiftRange{}, op,
    V.VM{mem, ds, cs, ip, control,                               out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.ShiftRange{}, ip}, out} : V.VM}

law atomic_opcode:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for op: U32
  {V.commit(V.Opcode{}, op,
    V.VM{mem, ds, cs, ip, control,                           out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.Opcode{}, ip}, out} : V.VM}

law atomic_device:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for op: U32
  {V.commit(V.Device{}, op,
    V.VM{mem, ds, cs, ip, control,                           out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.Device{}, ip}, out} : V.VM}

law advance_vo:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for reason: V.Fault
  for at: U32
  {V.advance(
    V.VM{mem, ds, cs, ip,             V.Control{V.Vo{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, U32.add(ip, 1), V.Control{V.Vo{}, reason, at}, out} : V.VM}

law advance_ru:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for reason: V.Fault
  for at: U32
  {V.advance(
    V.VM{mem, ds, cs, ip,             V.Control{V.Ru{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, U32.add(ip, 1), V.Control{V.Ru{}, reason, at}, out} : V.VM}

law advance_ti:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for reason: V.Fault
  for at: U32
  {V.advance(
    V.VM{mem, ds, cs, ip,             V.Control{V.Ti{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, U32.add(ip, 1), V.Control{V.Ti{}, reason, at}, out} : V.VM}

law zero_quantum:
  for vm: V.VM
  {V.run(0n, vm) == vm : V.VM}

law run_idle_vo:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for fuel: Nat
  for reason: V.Fault
  for at: U32
  {V.run(fuel,
    V.VM{mem, ds, cs, ip, V.Control{V.Vo{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Vo{}, reason, at}, out} : V.VM}

law run_idle_na:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for fuel: Nat
  for reason: V.Fault
  for at: U32
  {V.run(fuel,
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, reason, at}, out} : V.VM}

law run_idle_ti:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for fuel: Nat
  for reason: V.Fault
  for at: U32
  {V.run(fuel,
    V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Ti{}, reason, at}, out} : V.VM}

law fleet_isolation:
  for left: V.Fleet
  for right: V.Fleet
  {V.fleet_tick(V.Both{left, right}) ==
    V.Both{V.fleet_tick(left), V.fleet_tick(right)} : V.Fleet}

law valid_divide:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  for valid: {U32.is_ne(b, 0) == True{} : Bool}
  {V.execute(131,
    V.VM{mem, b <> a <> ds,         cs, ip, control, out}) ==
    V.VM{mem, V.divide(a, b) <> ds, cs, ip, control, out} : V.VM}

law valid_modulo:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  for valid: {U32.is_ne(b, 0) == True{} : Bool}
  {V.execute(132,
    V.VM{mem, b <> a <> ds,            cs, ip, control, out}) ==
    V.VM{mem, V.remainder(a, b) <> ds, cs, ip, control, out} : V.VM}

law valid_shift:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  for valid: {U32.is_lt(V.magnitude(b), 32) == True{} : Bool}
  {V.execute(133,
    V.VM{mem, b <> a <> ds,        cs, ip, control, out}) ==
    V.VM{mem, V.shift(a, b) <> ds, cs, ip, control, out} : V.VM}

law little_endian_store:
  for mem: Array<U32>
  for a: U32
  for v: U32
  {V.write_cell(mem, a, v) ==
    Array.set(U32,
      Array.set(U32,
        Array.set(U32,
          Array.set(U32, mem, a, U32.and(v, 255)),
          U32.add(a, 1), U32.and(U32.shrn(v, 8n), 255)),
        U32.add(a, 2), U32.and(U32.shrn(v, 16n), 255)),
      U32.add(a, 3), U32.and(U32.shrn(v, 24n), 255)) : Array<U32>}

law little_endian_load:
  for mem: Array<U32>
  for a: U32
  {V.read_cell(mem, a) ==
    V.pair(Array<U32>, U32, Array<U32> & U32, Array.get(U32, mem, a), mem => b0 =>
    V.pair(Array<U32>, U32, Array<U32> & U32, Array.get(U32, mem, U32.add(a, 1)), mem => b1 =>
    V.pair(Array<U32>, U32, Array<U32> & U32, Array.get(U32, mem, U32.add(a, 2)), mem => b2 =>
    V.pair(Array<U32>, U32, Array<U32> & U32, Array.get(U32, mem, U32.add(a, 3)), mem => b3 =>
      (mem, U32.or(U32.or(b0, U32.shln(b1, 8n)), U32.shln(U32.or(b2, U32.shln(b3, 8n)), 16n))))))) : Array<U32> & U32}

law division_fault:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  for invalid: {U32.is_ne(b, 0) == False{} : Bool}
  {V.execute(131,
    V.VM{mem, b <> a <> ds, cs, ip, control,                                out}) ==
    V.VM{mem, b <> a <> ds, cs, ip, V.Control{V.Na{}, V.ZeroDivisor{}, ip}, out} : V.VM}

law shift_fault:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for b: U32
  for invalid: {U32.is_lt(V.magnitude(b), 32) == False{} : Bool}
  {V.execute(133,
    V.VM{mem, b <> a <> ds, cs, ip, control,                               out}) ==
    V.VM{mem, b <> a <> ds, cs, ip, V.Control{V.Na{}, V.ShiftRange{}, ip}, out} : V.VM}

law byte_address_fault:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for invalid: {V.address_ok(a, 1) == False{} : Bool}
  {V.execute(146,
    V.VM{mem, a <> ds, cs, ip, control,                            out}) ==
    V.VM{mem, a <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law cell_address_fault:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for invalid: {V.address_ok(a, 4) == False{} : Bool}
  {V.execute(147,
    V.VM{mem, a <> ds, cs, ip, control,                            out}) ==
    V.VM{mem, a <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law byte_store_fault:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for v: U32
  for invalid: {V.address_ok(a, 1) == False{} : Bool}
  {V.execute(148,
    V.VM{mem, a <> v <> ds, cs, ip, control,                            out}) ==
    V.VM{mem, a <> v <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law cell_store_fault:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for a: U32
  for v: U32
  for invalid: {V.address_ok(a, 4) == False{} : Bool}
  {V.execute(149,
    V.VM{mem, a <> v <> ds, cs, ip, control,                            out}) ==
    V.VM{mem, a <> v <> ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law fetch_fault:
  for mem: Array<U32>
  for ds: List<&2, U32>
  for cs: List<&2, U32>
  for ip: U32
  for control: V.Control
  for out: String
  for invalid: {V.code_ok(ip) == False{} : Bool}
  {V.step(
    V.VM{mem, ds, cs, ip, control,                            out}) ==
    V.VM{mem, ds, cs, ip, V.Control{V.Na{}, V.Address{}, ip}, out} : V.VM}

law run_successor:
  for vm: V.VM
  for n: Nat
  {V.run(1n+n, vm) == V.run(n, V.tick(vm)) : V.VM}

