# B4 bytecode machine. Cells are two's-complement U32 bit patterns.
import Base

# vorunati: stable external state codes 0, 1, 2, 3.
type Mode is Data:
  Vo{}
  Ru{}
  Na{}
  Ti{}

type Fault is Data:
  Clear{}
  DataUnderflow{}
  ControlUnderflow{}
  Address{}
  ZeroDivisor{}
  ShiftRange{}
  Opcode{}
  Device{}

type Control is Data:
  Control{mode: Mode, fault: Fault, at: U32}

type VM is Type:
  VM{mem: Array<U32>, ds: List<&2, U32>, cs: List<&2, U32>, ip: U32,
     control: Control, output: String}


# Continuations unpack computed affine results without copying their arrays.
def pair(-A: Type, -B: Type, -R: Type, p: A & B, f: A -> B -> R) -> R:
  (a, b) = p
  f(a, b)

def state(-R: Type, vm: VM,
          f: Array<U32> -> List<&2, U32> -> List<&2, U32> -> U32 -> Control -> String -> R) -> R:
  VM{mem, ds, cs, ip, control, output} = vm
  f(mem, ds, cs, ip, control, output)

def pick(c: Bool, a: U32, b: U32) -> U32:
  Bool.pick(U32, c, a, b)

def negative(x: U32) -> Bool:
  U32.is_ge(x, 2147483648)

def negate(x: U32) -> U32:
  U32.sub(0, x)

def magnitude(+x: U32) -> U32:
  pick(negative(x), negate(x), x)

def signed_byte(+x: U32) -> U32:
  pick(U32.is_ge(x, 128), U32.or(x, 4294967040), x)

def truth(b: Bool) -> U32:
  pick(b, 4294967295, 0)

def signed_lt(a: U32, b: U32) -> Bool:
  U32.is_lt(U32.xor(a, 2147483648), U32.xor(b, 2147483648))

def divide(+a: U32, +b: U32) -> U32:
  +q = U32.div(magnitude(a), magnitude(b))
  pick(negative(U32.xor(a, b)), negate(q), q)

def remainder(+a: U32, b: U32) -> U32:
  +r = U32.mod(magnitude(a), magnitude(b))
  pick(negative(a), negate(r), r)

def shift_dir(right: Bool, a: U32, n: Nat) -> U32:
  match right:
    case True{}:
      U32.shrn(a, n)
    case False{}:
      U32.shln(a, n)

def shift_valid(ok: Bool, a: U32, +b: U32) -> U32:
  match ok:
    case False{}:
      0
    case True{}:
      shift_dir(negative(b), a, U32.to_nat(magnitude(b)))

def shift(a: U32, +b: U32) -> U32:
  shift_valid(U32.is_lt(magnitude(b), 32), a, b)

# Four byte reads preserve ownership of memory; independent byte packing
# and unpacking use Bend's parallel call notation.

def pack_half(a: U32, b: U32) -> U32:
  U32.or(a, U32.shln(b, 8n))

def read_cell(mem: Array<U32>, +a: U32) -> Array<U32> & U32:
  pair(Array<U32>, U32, Array<U32> & U32, mem[a], mem => b0 => (
    pair(Array<U32>, U32, Array<U32> & U32, mem[U32.add(a, 1)], mem => b1 => (
      pair(Array<U32>, U32, Array<U32> & U32, mem[U32.add(a, 2)], mem => b2 => (
        pair(Array<U32>, U32, Array<U32> & U32, mem[U32.add(a, 3)], mem => b3 => (
          lo hi = pack_half(b0, b1) pack_half(b2, b3)
          (mem, U32.or(lo, U32.shln(hi, 16n)))
        
        ))
      ))
    ))
  ))

def low_byte(x: U32) -> U32:
  U32.and(x, 255)

def high_byte(x: U32) -> U32:
  U32.and(U32.shrn(x, 8n), 255)

def write_half(mem: Array<U32>, +a: U32, +v: U32) -> Array<U32>:
  lo hi = low_byte(v) high_byte(v)
  mem[a] <- lo
  mem[U32.add(a, 1)] <- hi

def write_cell(mem: Array<U32>, +a: U32, +v: U32) -> Array<U32>:
  # The top byte is stored at a+3 so this matches little_endian_store.
  b2 b3 = U32.and(U32.shrn(v, 16n), 255) U32.and(U32.shrn(v, 24n), 255)
  mem = write_half(mem, a, v)
  mem[U32.add(a, 2)] <- b2
  mem[U32.add(a, 3)] <- b3

def boot() -> VM:
  mem = [0 : U32^16n]
  mem = write_cell(mem, 124, 256)
  VM{mem, [], [], 256, Control{Ti{}, Clear{}, 0}, ""}

def push(vm: VM, x: U32) -> VM:
  VM{mem, ds, cs, ip, control, output} = vm
  VM{mem, x <> ds, cs, ip, control, output}

# Internal microcode helper. Public execution validates stack depth first.

def pop(xs: List<&2, U32>) -> U32 & List<&2, U32>:
  match xs:
    case Nil{}:
      (0, [])
    case h <> t:
      (h, t)

def jump(+a: U32) -> U32:
  U32.sub(pick(signed_lt(a, 256), 256, a), 1)

def hop(mem: Array<U32>, +ip: U32) -> Array<U32> & U32:
  pair(Array<U32>, U32, Array<U32> & U32, mem[U32.add(ip, 1)], mem => b => (
    (mem, jump(U32.add(ip, signed_byte(b))))
  
  ))

def next_counter(+x: U32) -> U32:
  pick(signed_lt(0, x), U32.sub(x, 1), x)

def next_jump(zero: Bool, mem: Array<U32>, ds: List<&2, U32>, cs: List<&2, U32>,
              counter: U32, ip: U32, control: Control, output: String) -> VM:
  match zero:
    case True{}:
      VM{mem, ds, cs, U32.add(ip, 1), control, output}
    case False{}:
      pair(Array<U32>, U32, VM, hop(mem, ip), mem => target => (
        VM{mem, ds, counter <> cs, target, control, output}
  
      ))

def conditional_hop(zero: Bool, mem: Array<U32>, ip: U32) -> Array<U32> & U32:
  match zero:
    case True{}:
      hop(mem, ip)
    case False{}:
      (mem, U32.add(ip, 1))

def device(cmd: U32, vm: VM) -> VM:
  match cmd:
    case 101:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), ch => ds => (
        VM{mem, ds, cs, ip, control, output ++ SCon{Chr{ch}, SNil{}}}
      ))
    case _:
      VM{mem, ds, cs, +ip, control, output} = vm
      VM{mem, ds, cs, ip, Control{Na{}, Device{}, ip}, output}

def register_op(group: U32, +r: U32, vm: VM) -> VM:
  match group:
    case 0:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(Array<U32>, U32, VM, read_cell(mem, r), mem => target => (
        VM{mem, ds, U32.add(ip, 1) <> cs, jump(target), control, output}
      ))
    case 1:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(Array<U32>, U32, VM, read_cell(mem, r), mem => value => (
        VM{mem, value <> ds, cs, ip, control, output}
      ))
    case 2:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), value => ds => (
        VM{write_cell(mem, r, value), ds, cs, ip, control, output}
      ))
    case _:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), delta => ds => (
        pair(Array<U32>, U32, VM, read_cell(mem, r), mem => +value => (
          mem = write_cell(mem, r, U32.add(value, delta))
          VM{mem, value <> ds, cs, ip, control, output}
    
        ))
      ))

def register_or_reserved(is_register: Bool, +op: U32, vm: VM) -> VM:
  match is_register:
    case True{}:
      register_op(U32.div(op, 32), U32.mul(U32.mod(op, 32), 4), vm)
    case False{}:
      vm

def return_control(top: Bool, control: Control) -> Control:
  match top:
    case True{}:
      Control{Ti{}, Clear{}, 0}
    case False{}:
      control

# $A0..$A5 keep IEEE binary32 bit patterns in ordinary stack cells.

def f32_of(+bits: U32) -> F32:
  U32{w} = bits
  F32{w}

def f_bits(x: F32) -> U32:
  F32.bits(x)

def fadd(a: U32, b: U32) -> U32:
  f_bits(F32.add(f32_of(a), f32_of(b)))

def fsub(a: U32, b: U32) -> U32:
  f_bits(F32.sub(f32_of(a), f32_of(b)))

def fmul(a: U32, b: U32) -> U32:
  f_bits(F32.mul(f32_of(a), f32_of(b)))

def fdiv(a: U32, b: U32) -> U32:
  f_bits(F32.div(f32_of(a), f32_of(b)))

def flt(a: U32, b: U32) -> U32:
  truth(F32.is_lt(f32_of(a), f32_of(b)))

def itof_signed(neg: Bool, +n: U32) -> F32:
  match neg:
    case False{}:
      U32.to_f32(n)
    case True{}:
      F32.neg(U32.to_f32(negate(n)))

def itof_go(+n: U32) -> F32:
  itof_signed(negative(n), n)

def itof(n: U32) -> U32:
  f_bits(itof_go(n))

def push_calc(vm: VM, y: U32) -> VM:
  VM{mem, ds, cs, ip, control, output} = vm
  VM{mem, y <> ds, cs, ip, control, output}

def calc2(vm: VM, f: U32 -> U32 -> U32) -> VM:
  VM{mem, ds, cs, ip, control, output} = vm
  pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
    pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
      push_calc(VM{mem, ds, cs, ip, control, output}, f(a, b))
    ))
  ))

def calc1(vm: VM, f: U32 -> U32) -> VM:
  VM{mem, ds, cs, ip, control, output} = vm
  pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
    push_calc(VM{mem, ds, cs, ip, control, output}, f(a))
  ))

def microcode(op: U32, vm: VM) -> VM:
  match op:
    case 0:
      vm
    case 128:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, U32.add(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 129:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, U32.sub(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 130:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, U32.mul(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 131:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, divide(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 132:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, remainder(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 133:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, shift(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 134:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, U32.and(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 135:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, U32.or(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 136:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, U32.xor(a, b) <> ds, cs, ip, control, output}
        ))
      ))
    case 138:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, truth(U32.is_eq(a, b)) <> ds, cs, ip, control, output}
        ))
      ))
    case 139:
      VM{mem, ds, cs, ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, truth(signed_lt(a, b)) <> ds, cs, ip, control, output}
        ))
      ))
    case 137:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        VM{mem, U32.not(a) <> ds, cs, ip, control, output}
      ))
    case 140:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), +a => ds => (
        VM{mem, a <> a <> ds, cs, ip, control, output}
      ))
    case 141:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
          VM{mem, a <> b <> ds, cs, ip, control, output}
        ))
      ))
    case 142:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), b => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), +a => ds => (
          VM{mem, a <> b <> a <> ds, cs, ip, control, output}
        ))
      ))
    case 143:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        VM{mem, ds, cs, ip, control, output}
      ))
    case 144:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        VM{mem, ds, a <> cs, ip, control, output}
      ))
    case 145:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(cs), a => cs => (
        VM{mem, a <> ds, cs, ip, control, output}
      ))
    case 146:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        pair(Array<U32>, U32, VM, mem[a], mem => v => (
          VM{mem, v <> ds, cs, ip, control, output}
        ))
      ))
    case 147:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        pair(Array<U32>, U32, VM, read_cell(mem, a), mem => v => (
          VM{mem, v <> ds, cs, ip, control, output}
        ))
      ))
    case 148:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), v => ds => (
          mem[a] <- low_byte(v)
          VM{mem, ds, cs, ip, control, output}
        ))
      ))
    case 149:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        pair(U32, List<&2, U32>, VM, pop(ds), v => ds => (
          VM{write_cell(mem, a, v), ds, cs, ip, control, output}
        ))
      ))
    case 150:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(Array<U32>, U32, VM, mem[U32.add(ip, 1)], mem => v => (
        VM{mem, v <> ds, cs, U32.add(ip, 1), control, output}
      ))
    case 151:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(Array<U32>, U32, VM, read_cell(mem, U32.add(ip, 1)), mem => v => (
        VM{mem, v <> ds, cs, U32.add(ip, 4), control, output}
      ))
    case 152:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        pair(Array<U32>, U32, VM, mem[a], mem => v => (
          VM{mem, signed_byte(v) <> ds, cs, ip, control, output}
        ))
      ))
    case 153:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(Array<U32>, U32, VM, mem[U32.add(ip, 1)], mem => v => (
        VM{mem, signed_byte(v) <> ds, cs, U32.add(ip, 1), control, output}
      ))
    case 154:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(Array<U32>, U32, VM, read_cell(mem, U32.add(ip, 1)), mem => target => (
        VM{mem, ds, cs, jump(target), control, output}
      ))
    case 155:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(Array<U32>, U32, VM, hop(mem, ip), mem => target => (
        VM{mem, ds, cs, target, control, output}
      ))
    case 156:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), a => ds => (
        pair(Array<U32>, U32, VM, conditional_hop(U32.is_zero(a), mem, ip), mem => target => (
          VM{mem, ds, cs, target, control, output}
        ))
      ))
    case 157:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(Array<U32>, U32, VM, read_cell(mem, U32.add(ip, 1)), mem => target => (
        VM{mem, ds, U32.add(ip, 5) <> cs, jump(target), control, output}
      ))
    case 158:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(cs), +a => cs => (
        VM{mem, ds, cs, pick(U32.is_zero(a), ip, U32.sub(a, 1)), return_control(U32.is_zero(a), control), output}
      ))
    case 159:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(cs), a => cs => (
        +n = next_counter(a)
        next_jump(U32.is_zero(n), mem, ds, cs, n, ip, control, output)
      ))
    case 160:
      calc2(vm, fadd)
    case 161:
      calc2(vm, fsub)
    case 162:
      calc2(vm, fmul)
    case 163:
      calc2(vm, fdiv)
    case 164:
      calc2(vm, flt)
    case 165:
      calc1(vm, itof)
    case 253:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(U32, List<&2, U32>, VM, pop(ds), cmd => ds => (
        device(cmd, VM{mem, ds, cs, ip, control, output})
      ))
    case 254:
      VM{mem, ds, cs, +ip, control, output} = vm
      VM{mem, ds, cs, ip, Control{Ti{}, Clear{}, 0}, output}
    case 255:
      VM{mem, ds, cs, +ip, control, output} = vm
      VM{mem, ds, cs, ip, Control{Ti{}, Clear{}, 0}, output}
    case 192:
      push(vm, 0)
    case 193:
      push(vm, 1)
    case 246:
      push(vm, 2)
    case 247:
      push(vm, 4294967295)
    case 248:
      push(vm, 4)
    case +other:
      register_or_reserved(U32.is_lt(other, 128), other, vm)

def mode_code(mode: Mode) -> U32:
  match mode:
    case Vo{}:
      0
    case Ru{}:
      1
    case Na{}:
      2
    case Ti{}:
      3

def fault(vm: VM, reason: Fault) -> VM:
  VM{mem, ds, cs, +ip, control, output} = vm
  VM{mem, ds, cs, ip, Control{Na{}, reason, ip}, output}

def set_mode(vm: VM, mode: Mode) -> VM:
  VM{mem, ds, cs, ip, control, output} = vm
  VM{mem, ds, cs, ip, Control{mode, Clear{}, 0}, output}

def enough(n: Nat, xs: List<&2, U32>) -> Bool:
  match n xs:
    case 0n _:
      True{}
    case 1n+p Nil{}:
      False{}
    case 1n+p h <> t:
      enough(p, t)

def need_data(op: U32) -> Nat:
  match op:
    case 128:
      2n
    case 129:
      2n
    case 130:
      2n
    case 131:
      2n
    case 132:
      2n
    case 133:
      2n
    case 134:
      2n
    case 135:
      2n
    case 136:
      2n
    case 137:
      1n
    case 138:
      2n
    case 139:
      2n
    case 140:
      1n
    case 141:
      2n
    case 142:
      2n
    case 143:
      1n
    case 144:
      1n
    case 146:
      1n
    case 147:
      1n
    case 148:
      2n
    case 149:
      2n
    case 152:
      1n
    case 156:
      1n
    case 160:
      2n
    case 161:
      2n
    case 162:
      2n
    case 163:
      2n
    case 164:
      2n
    case 165:
      1n
    case 253:
      2n
    case +other:
      Bool.pick(Nat, Bool.and(U32.is_ge(other, 64), U32.is_lt(other, 128)), 1n, 0n)

def need_control(op: U32) -> Nat:
  match op:
    case 145:
      1n
    case 158:
      1n
    case 159:
      1n
    case _:
      0n

def supported(+op: U32) -> Bool:
  Bool.or(U32.is_lt(op, 166),
    Bool.or(U32.is_eq(op, 192), Bool.or(U32.is_eq(op, 193),
      Bool.or(Bool.and(U32.is_ge(op, 246), U32.is_le(op, 248)),
        Bool.and(U32.is_ge(op, 253), U32.is_le(op, 255))))))

# Memory has 65536 bytes. No read or write may wrap around its end.

def address_ok(+addr: U32, width: U32) -> Bool:
  U32.is_le(addr, U32.sub(65536, width))

def code_ok(+addr: U32) -> Bool:
  Bool.and(U32.is_ge(addr, 256), U32.is_lt(addr, 65536))

def check(ok: Bool, reason: Fault) -> Fault:
  match ok:
    case True{}:
      Clear{}
    case False{}:
      reason

def top(xs: List<&2, U32>) -> U32:
  match xs:
    case Nil{}:
      0
    case h <> t:
      h

# Inspecting an immediate hands memory back unchanged. Target bounds are
# checked before any stack or memory mutation, including a call's return push.

def check_target(ok: Bool, mem: Array<U32>, addr: U32) -> Array<U32> & Fault:
  match ok:
    case False{}:
      (mem, Address{})
    case True{}:
      pair(Array<U32>, U32, Array<U32> & Fault, read_cell(mem, addr), mem => target => (
        (mem, check(code_ok(target), Address{}))
      ))

def check_hop(ok: Bool, mem: Array<U32>, +ip: U32) -> Array<U32> & Fault:
  match ok:
    case False{}:
      (mem, Address{})
    case True{}:
      pair(Array<U32>, U32, Array<U32> & Fault, mem[U32.add(ip, 1)], mem => offset => (
        (mem, check(code_ok(U32.add(ip, signed_byte(offset))), Address{}))
      ))

def branch_check(taken: Bool, mem: Array<U32>, +ip: U32) -> Array<U32> & Fault:
  match taken:
    case True{}:
      check_hop(U32.is_lt(ip, 65535), mem, ip)
    case False{}:
      (mem, check(U32.is_lt(ip, 65535), Address{}))

def register_target(invoke: Bool, mem: Array<U32>, op: U32) -> Array<U32> & Fault:
  match invoke:
    case True{}:
      check_target(True{}, mem, U32.mul(op, 4))
    case False{}:
      (mem, Clear{})

# Validation precedence: unknown opcode, data stack, control stack, operands.

def operands(op: U32, mem: Array<U32>, +ds: List<&2, U32>, cs: List<&2, U32>, +ip: U32) -> Array<U32> & Fault:
  match op:
    case 131:
      (mem, check(U32.is_ne(top(ds), 0), ZeroDivisor{}))
    case 132:
      (mem, check(U32.is_ne(top(ds), 0), ZeroDivisor{}))
    case 133:
      (mem, check(U32.is_lt(magnitude(top(ds)), 32), ShiftRange{}))
    case 146:
      (mem, check(address_ok(top(ds), 1), Address{}))
    case 147:
      (mem, check(address_ok(top(ds), 4), Address{}))
    case 148:
      (mem, check(address_ok(top(ds), 1), Address{}))
    case 149:
      (mem, check(address_ok(top(ds), 4), Address{}))
    case 150:
      (mem, check(U32.is_lt(ip, 65535), Address{}))
    case 151:
      (mem, check(U32.is_lt(ip, 65532), Address{}))
    case 152:
      (mem, check(address_ok(top(ds), 1), Address{}))
    case 153:
      (mem, check(U32.is_lt(ip, 65535), Address{}))
    case 154:
      # b4-tests.org op.jm takes any readable target. jump clamps below 256.
      (mem, check(U32.is_lt(ip, 65532), Address{}))
    case 155:
      check_hop(U32.is_lt(ip, 65535), mem, ip)
    case 156:
      branch_check(U32.is_zero(top(ds)), mem, ip)
    case 157:
      check_target(U32.is_lt(ip, 65532), mem, U32.add(ip, 1))
    case 158:
      +addr = top(cs)
      (mem, check(Bool.or(U32.is_zero(addr), code_ok(addr)), Address{}))
    case 159:
      branch_check(U32.is_ne(next_counter(top(cs)), 0), mem, ip)
    case 253:
      (mem, check(U32.is_eq(top(ds), 101), Device{}))
    case +other:
      register_target(Bool.and(U32.is_gt(other, 0), U32.is_lt(other, 32)), mem, other)

def stack_check(known: Bool, data: Bool, ctrl: Bool) -> Fault:
  match known data ctrl:
    case False{} _ _:
      Opcode{}
    case True{} False{} _:
      DataUnderflow{}
    case True{} True{} False{}:
      ControlUnderflow{}
    case True{} True{} True{}:
      Clear{}

def commit(reason: Fault, op: U32, vm: VM) -> VM:
  match reason:
    case Clear{}:
      microcode(op, vm)
    case other:
      fault(vm, other)

def validated(reason: Fault, +op: U32, vm: VM) -> VM:
  match reason:
    case Clear{}:
      VM{mem, +ds, +cs, +ip, control, output} = vm
      pair(Array<U32>, Fault, VM, operands(op, mem, ds, cs, ip), mem => reason => (
        commit(reason, op, VM{mem, ds, cs, ip, control, output})
      ))
    case other:
      fault(vm, other)

def execute(+op: U32, vm: VM) -> VM:
  VM{mem, +ds, +cs, ip, control, output} = vm
  validated(stack_check(supported(op), enough(need_data(op), ds), enough(need_control(op), cs)),
    op, VM{mem, ds, cs, ip, control, output})

def advance(vm: VM) -> VM:
  match vm:
    case VM{mem, ds, cs, ip, Control{Na{}, reason, at}, output}:
      VM{mem, ds, cs, ip, Control{Na{}, reason, at}, output}
    case VM{mem, ds, cs, ip, control, output}:
      VM{mem, ds, cs, U32.add(ip, 1), control, output}

def fetch(ok: Bool, vm: VM) -> VM:
  match ok:
    case False{}:
      fault(vm, Address{})
    case True{}:
      VM{mem, ds, cs, +ip, control, output} = vm
      pair(Array<U32>, U32, VM, mem[ip], mem => op => (
        advance(execute(op, VM{mem, ds, cs, ip, control, output}))
      ))

def step(vm: VM) -> VM:
  VM{mem, ds, cs, +ip, control, output} = vm
  fetch(code_ok(ip), VM{mem, ds, cs, ip, control, output})

# Scheduler ticks cannot start an offline, stopped, or faulted machine.

def tick(vm: VM) -> VM:
  match vm:
    case VM{mem, ds, cs, ip, Control{Ru{}, reason, at}, output}:
      step(VM{mem, ds, cs, ip, Control{Ru{}, reason, at}, output})
    case VM{mem, ds, cs, ip, control, output}:
      VM{mem, ds, cs, ip, control, output}

# Running out of a scheduling quantum yields a still-running VM; it is not
# a VM fault. Hosts can schedule it again, inspect it, or take it offline.

def run(fuel: Nat, vm: VM) -> VM:
  match fuel vm:
    case 0n vm:
      vm
    case 1n+left VM{mem, ds, cs, ip, Control{Ru{}, reason, at}, output}:
      run(left, tick(VM{mem, ds, cs, ip, Control{Ru{}, reason, at}, output}))
    case 1n+left VM{mem, ds, cs, ip, control, output}:
      VM{mem, ds, cs, ip, control, output}

# Balanced groups are schedulable in parallel without sharing VM memory.
type Fleet is Type:
  One{vm: VM}
  Both{left: Fleet, right: Fleet}

def fleet_tick(fleet: Fleet) -> Fleet:
  match fleet:
    case One{vm}:
      One{tick(vm)}
    case Both{left, right}:
      a b = fleet_tick(left) fleet_tick(right)
      Both{a, b}

