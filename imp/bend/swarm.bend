# Parallel search on a swarm of B4 machines.
#
# Every machine runs the same three instructions, du ml sb:
#   duplicate the guess, square it, subtract from the key.
# The machine whose stack top is 0 held the square root.
# fleet_run! sends that search to the GPU (16384 lanes per wave).
import Base
import ./vm.bend as V

def key() -> U32:
  10000

def depth() -> Nat:
  14n

def span(d: Nat) -> U32:
  match d:
    case 0n:
      1
    case 1n+p:
      U32.mul(span(p), 2)

def program() -> Array<U32>:
  mem = [0 : U32^9n]
  mem[256] <- 140
  mem[257] <- 130
  mem[258] <- 129
  mem

def machine(guess: U32) -> V.VM:
  V.VM{program(), guess <> key() <> [], [], 256,
    V.Control{V.Ru{}, V.Clear{}, 0}, ""}

def fork(+d: Nat, +guess: U32) -> V.Fleet:
  match d:
    case 0n:
      V.One{machine(guess)}
    case 1n+p:
      step = span(p)
      left right = fork(p, guess) fork(p, U32.add(guess, step))
      V.Both{left, right}

def fleet_run(+fuel: Nat, fleet: V.Fleet) -> V.Fleet:
  match fleet:
    case V.One{vm}:
      V.One{V.run(fuel, vm)}
    case V.Both{left, right}:
      a b = fleet_run(fuel, left) fleet_run(fuel, right)
      V.Both{a, b}

def hit(vm: V.VM, guess: U32) -> String:
  V.VM{mem, ds, cs, ip, control, output} = vm
  match ds:
    case 0 <> rest:
      "machine " ++ U32.show(guess) ++ "\n"
    case _:
      ""

def report(fleet: V.Fleet, +guess: U32, count: U32) -> String:
  match fleet:
    case V.One{vm}:
      hit(vm, guess)
    case V.Both{left, right}:
      +half = U32.shrn(count, 1n)
      a b = report(left, guess, half) report(right, U32.add(guess, half), half)
      a ++ b

def main() -> IO(Unit):
  found = report(fleet_run!(3n, fork(depth(), 0)), 0, span(depth()))
  IO.print(found)
