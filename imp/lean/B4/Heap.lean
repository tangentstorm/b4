/-!
# A heap of blocks: the allocator's model

The model of `b4a/mm.b4a.org`'s allocator, on a list of blocks, each a header
of `h` units followed by its data. `alloc h sp n` takes the first free block
big enough for `n`, merging each free block it meets with the free blocks after
it, and splitting off what is left when that is at least `sp` units; `free h o`
frees the block at offset `o`.

It is stated once, for any header and split threshold, and used twice:
`B4.MM` counts in bytes (`h = 12`, `sp = 16`), and the allocator rewritten in
Hehner's language counts in cells of four bytes (`h = 3`, `sp = 4`).
`alloc_scale` relates the two: counted in units `k` times smaller, the same
heap does the same thing.
-/

namespace B4.Heap

/-- A block: the size of its data, and whether it is used. -/
structure Blk where
  /-- The size of its data. -/
  size : Nat
  /-- Whether it is used. -/
  used : Bool
  deriving Repr, DecidableEq, Inhabited

/-- Absorb the free blocks at the front of `bs` into a free block of `size`. -/
def absorb (h size : Nat) : List Blk → Nat × List Blk
  | b :: bs => if b.used then (size, b :: bs) else absorb h (size + h + b.size) bs
  | [] => (size, [])

theorem absorb_length (h size : Nat) : ∀ bs : List Blk, (absorb h size bs).2.length ≤ bs.length
  | [] => by simp [absorb]
  | b :: bs => by
    unfold absorb
    split
    · simp
    · have := absorb_length h (size + h + b.size) bs; simp only [List.length_cons]; omega

/-- Take a free block of `size` for `n`, splitting off the rest when it is at
least `sp`. -/
def claim (h sp n size : Nat) : List Blk :=
  if n + sp ≤ size then [⟨n, true⟩, ⟨size - n - h, false⟩] else [⟨size, true⟩]

/-- **Allocation, on the blocks**: the offset of the block taken, if one is big
enough, and the blocks after — merged as far as the search went, even when none
is. -/
def alloc (h sp n : Nat) : List Blk → Option Nat × List Blk
  | [] => (none, [])
  | b :: bs =>
    if b.used then
      let r := alloc h sp n bs
      (r.1.map (· + h + b.size), b :: r.2)
    else
      have := absorb_length h b.size bs
      if n ≤ (absorb h b.size bs).1 then
        (some 0, claim h sp n (absorb h b.size bs).1 ++ (absorb h b.size bs).2)
      else
        let r := alloc h sp n (absorb h b.size bs).2
        (r.1.map (· + h + (absorb h b.size bs).1), ⟨(absorb h b.size bs).1, false⟩ :: r.2)
termination_by bs => bs.length
decreasing_by all_goals simp only [List.length_cons]; omega

/-- **Freeing, on the blocks**: the block at offset `o` is free. -/
def free (h o : Nat) : List Blk → List Blk
  | [] => []
  | b :: bs => if o = 0 then { b with used := false } :: bs else b :: free h (o - h - b.size) bs

/-! ### Changing the unit -/

/-- A block counted in units `k` times smaller. -/
def Blk.scale (k : Nat) (b : Blk) : Blk := ⟨k * b.size, b.used⟩

theorem absorb_scale (k h : Nat) : ∀ (size : Nat) (bs : List Blk),
    absorb (k * h) (k * size) (bs.map (Blk.scale k)) =
      (k * (absorb h size bs).1, (absorb h size bs).2.map (Blk.scale k))
  | size, [] => by simp [absorb]
  | size, b :: bs => by
    by_cases hu : b.used
    · simp [absorb, hu, Blk.scale]
    · have := absorb_scale k h (size + h + b.size) bs
      simp only [List.map_cons, absorb, Blk.scale, hu, Bool.false_eq_true, ite_false] at this ⊢
      rw [← this, Nat.mul_add, Nat.mul_add]

theorem claim_scale {k : Nat} (hk : 0 < k) (h sp n size : Nat) :
    claim (k * h) (k * sp) (k * n) (k * size) = (claim h sp n size).map (Blk.scale k) := by
  unfold claim
  by_cases hs : n + sp ≤ size
  · have : k * n + k * sp ≤ k * size := by rw [← Nat.mul_add]; exact Nat.mul_le_mul_left k hs
    simp only [hs, this, ite_true, List.map_cons, List.map_nil, Blk.scale]
    congr
    rw [Nat.mul_sub, Nat.mul_sub]
  · have : ¬ k * n + k * sp ≤ k * size := by
      rw [← Nat.mul_add]; exact fun h' => hs (Nat.le_of_mul_le_mul_left h' hk)
    simp [hs, this, Blk.scale]

/-- **Counted in units `k` times smaller, the heap does the same**: allocation of
`k n` in the heap of `k`-times blocks is allocation of `n`, with the offset and
the blocks `k` times. -/
theorem alloc_scale {k : Nat} (hk : 0 < k) (h sp n : Nat) : ∀ bs : List Blk,
    alloc (k * h) (k * sp) (k * n) (bs.map (Blk.scale k)) =
      ((alloc h sp n bs).1.map (k * ·), (alloc h sp n bs).2.map (Blk.scale k))
  | [] => by simp [alloc]
  | b :: bs => by
    have hle : ∀ a c : Nat, k * a ≤ k * c ↔ a ≤ c := fun a c =>
      ⟨fun h' => Nat.le_of_mul_le_mul_left h' hk, fun h' => Nat.mul_le_mul_left k h'⟩
    by_cases hu : b.used
    · have ih := alloc_scale hk h sp n bs
      rw [List.map_cons, alloc, alloc]
      simp only [Blk.scale, hu, ite_true] at ih ⊢
      rw [ih]
      simp only [Option.map_map, Prod.mk.injEq, List.map_cons, Blk.scale, hu]
      refine ⟨?_, trivial⟩
      congr 1; funext x; simp [Function.comp, Nat.mul_add]
    · have ha := absorb_scale k h b.size bs
      have hlen := absorb_length h b.size bs
      have ih := alloc_scale hk h sp n (absorb h b.size bs).2
      rw [List.map_cons, alloc, alloc]
      simp only [Blk.scale, hu, Bool.false_eq_true, ite_false] at ha ih ⊢
      rw [ha]
      simp only [hle]
      by_cases hn : n ≤ (absorb h b.size bs).1
      · simp [hn, claim_scale hk]
      · simp only [hn, ite_false]
        rw [ih]
        simp only [Option.map_map, Prod.mk.injEq, List.map_cons, Blk.scale]
        refine ⟨?_, trivial⟩
        congr 1; funext x; simp [Function.comp, Nat.mul_add]
termination_by bs => bs.length
decreasing_by all_goals simp_all only [List.length_cons]; omega

end B4.Heap
