import B4.MM
open B4 B4.MM

/-! The allocator in b4a against its model, on random allocations and frees. -/

def H : UInt32 := 0x1000

def heap (s : State) : List Blk := blocks s 1000 H.toNat

/-- Run `k` random operations (from seed `g`) on the machine and on the model;
count the steps where they disagree. -/
def check : Nat → Nat → State → List Blk → List Nat → Nat
  | 0, _, _, _, _ => 0
  | k + 1, g, s, m, live =>
    let g := (g * 1103515245 + 12345) % 2147483648
    if g % 3 = 0 && !live.isEmpty then
      let a := live[(g / 3) % live.length]!
      let s' := clear (call s "mm-free" [a.toUInt32]).1
      let m' := free (a - H.toNat - 12) m
      (if heap s' == m' then 0 else 1) + check k g s' m' (live.erase a)
    else
      let n := (g / 7) % 200
      let (s', ds) := call s "mm-alloc" [n.toUInt32]
      let a := (ds.getLast?.getD 0).toNat
      let s' := clear s'
      match alloc (round n) m with
      | (some o, m') =>
        (if a == H.toNat + o + 12 && heap s' == m' then 0 else 1) + check k g s' m' (live ++ [a])
      | (none, m') => (if a == 0 && heap s' == m' then 0 else 1) + check k g s' m' live

-- 0 disagreements in 2000 random operations
#eval check 2000 42 (clear (call boot "mm-init" [H, 4000]).1) [⟨4000, false⟩] []
