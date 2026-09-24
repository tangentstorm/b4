# A compiled parallel scheduler check, independent of the console.
import Base
import ./vm.bend as V

def machine(mode: V.Mode, opcode: U32) -> V.VM:
  mem = [0 : U32^16n]
  mem[256] <- opcode
  V.VM{mem, [], [], 256, V.Control{mode, V.Clear{}, 0}, ""}

def mode_of(vm: V.VM) -> U32:
  V.VM{mem, ds, cs, ip, V.Control{mode, reason, at}, out} = vm
  V.mode_code(mode)

def show_machine(vm: V.VM) -> String:
  V.VM{mem, ds, cs, ip, V.Control{mode, reason, at}, out} = vm
  U32.show(V.mode_code(mode)) ++ ":" ++ U32.show(ip) ++ ":" ++ U32.show(V.top(ds))

def show_fleet(fleet: V.Fleet) -> String:
  match fleet:
    case V.One{vm}:
      show_machine(vm)
    case V.Both{left, right}:
      a b = show_fleet(left) show_fleet(right)
      a ++ " " ++ b

def main() -> IO(Unit):
  fleet = V.Both{
    V.Both{V.One{machine(V.Vo{}, 193)}, V.One{machine(V.Ru{}, 193)}},
    V.Both{V.One{machine(V.Ru{}, 128)}, V.One{machine(V.Ti{}, 193)}}}
  IO.print(show_fleet(V.fleet_tick(V.fleet_tick(fleet))))
