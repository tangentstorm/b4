import B4.AsmSyntax
import B4.Theory

/-!
# mm: a memory allocator in b4a

The design of `b4a/mm.b4a.org`: the heap is a list of blocks, each a 12-byte
header — `+0` the next block (`0` at the last), `+4` the size of its data, `+8`
whether it is used (`0` if free) — followed by its data.

* `mm-init ( a size -- )` makes the heap one free block of `size` bytes at `a`;
* `mm-alloc ( n -- a )` rounds `n` up to whole cells and takes the first free
  block big enough, splitting off what is left when that is at least 16 bytes;
  while it looks, it merges each free block it meets with the free blocks after
  it. It answers the address of the data, or `0` if no block is big enough.
* `mm-free ( a -- )` marks the block free again.

`MM.Blk` and `MM.alloc` are the same algorithm on a list of blocks, the model the
proofs (`B4.MMTheory`) relate the code to.
-/

namespace B4.MM

/-- The allocator, in b4a. -/
def code : Asm.Program := b4a! r#"
:mm-heap $00000000                    # the first block
:mm-n    $00000000                    # the size wanted
:mm-p    $00000000                    # the block in hand

:mm-init                              # ( a size -- ) one free block at a
  ov !mm-heap
  ov c4 ad wi                         # size(a) := size
  du c0 sw wi                         # next(a) := 0
  c0 sw lb 08 ad wi rt                # used(a) := 0

:mm-merge                             # ( -- ) absorb the free blocks after p
  .w @mm-p ri du .i lb 08 ad ri c0 eq .e .t .d
    @mm-p ri du c4 ad ri lb 0C ad     # ( q size(q)+12 )
    @mm-p c4 ad ri ad                 # ( q size )
    @mm-p c4 ad wi                    # size(p) := size
    ri @mm-p wi                       # next(p) := next(q)
  .o rt

:mm-claim                             # ( -- ) take p, splitting off the rest
  @mm-p c4 ad ri @mm-n sb lb 10 lt nt
  .i
    @mm-p lb 0C ad @mm-n ad           # ( q ) q = p + 12 + n
    @mm-p ri ov wi                    # next(q) := next(p)
    @mm-p c4 ad ri @mm-n sb lb 0C sb  # ( q size(p)-n-12 )
    ov c4 ad wi                       # size(q) := that
    c0 ov lb 08 ad wi                 # used(q) := 0
    @mm-p wi                          # next(p) := q
    @mm-n @mm-p c4 ad wi              # size(p) := n
  .t
  c1 @mm-p lb 08 ad wi rt             # used(p) := 1

:mm-alloc                             # ( n -- a )
  lb 03 ad ls FC an !mm-n             # n := n rounded up to whole cells
  @mm-heap !mm-p
  .w @mm-p .d
    @mm-p lb 08 ad ri c0 eq
    .i
      mm-merge
      @mm-p c4 ad ri @mm-n lt nt
      .i mm-claim @mm-p lb 0C ad rt .t
    .t
    @mm-p ri !mm-p                    # p := next(p)
  .o
  c0 rt

:mm-free                              # ( a -- )
  c0 sw lb 0C sb lb 08 ad wi rt       # used(a - 12) := 0
"#

/-! ### The model -/

/-- A block: the size of its data, and whether it is used. -/
structure Blk where
  size : Nat
  used : Bool
  deriving Repr, DecidableEq, Inhabited

/-- Absorb the free blocks at the front of `bs` into a free block of `size`. -/
def absorb (size : Nat) : List Blk → Nat × List Blk
  | b :: bs => if b.used then (size, b :: bs) else absorb (size + 12 + b.size) bs
  | [] => (size, [])

theorem absorb_length (size : Nat) : ∀ bs : List Blk, (absorb size bs).2.length ≤ bs.length
  | [] => by simp [absorb]
  | b :: bs => by
    unfold absorb
    split
    · simp
    · have := absorb_length (size + 12 + b.size) bs; simp only [List.length_cons]; omega

/-- Take a free block of `size` for `n`, splitting off the rest when it is at
least 16 bytes. -/
def claim (n size : Nat) : List Blk :=
  if size ≥ n + 16 then [⟨n, true⟩, ⟨size - n - 12, false⟩] else [⟨size, true⟩]

/-- **`mm-alloc`, on the blocks**: the offset of the block taken from the start
of the heap, if one is big enough, and the blocks after — merged as far as the
search went, even when none is. -/
def alloc (n : Nat) : List Blk → Option Nat × List Blk
  | [] => (none, [])
  | b :: bs =>
    if b.used then
      let r := alloc n bs
      (r.1.map (· + 12 + b.size), b :: r.2)
    else
      have := absorb_length b.size bs
      if n ≤ (absorb b.size bs).1 then (some 0, claim n (absorb b.size bs).1 ++ (absorb b.size bs).2)
      else
        let r := alloc n (absorb b.size bs).2
        (r.1.map (· + 12 + (absorb b.size bs).1), ⟨(absorb b.size bs).1, false⟩ :: r.2)
termination_by bs => bs.length
decreasing_by all_goals simp only [List.length_cons]; omega

/-- **`mm-free`, on the blocks**: the block at offset `o` is free. -/
def free (o : Nat) : List Blk → List Blk
  | [] => []
  | b :: bs => if o = 0 then { b with used := false } :: bs else b :: free (o - 12 - b.size) bs

/-- Round up to whole cells. -/
def round (n : Nat) : Nat := (n + 3) / 4 * 4

/-! ### Reading the heap back -/

/-- The blocks in a machine's memory, from block `a`, at most `fuel` of them. -/
def blocks (s : State) : Nat → Nat → List Blk
  | 0, _ => []
  | fuel + 1, a =>
    if a = 0 then [] else
      ⟨(getVal s.mem (a + 4)).toNat, getVal s.mem (a + 8) != 0⟩ ::
        blocks s fuel (getVal s.mem a).toNat

/-- A machine with the allocator loaded, its stacks empty. -/
def boot : State := setRST (code.load mkInitialState) 1

/-- Call a routine with arguments, and give the stack after. -/
def call (s : State) (name : String) (args : List UInt32) : State × List UInt32 :=
  let s := args.foldl dpush s
  let s := cpush s 0
  let s := setRST (setIP s (code.addr name)) 1
  let s := run s
  (s, dstack s)

/-- Pop the whole data stack. -/
def clear (s : State) : State := (List.range (getDSH s)).foldl (fun s _ => (dpop s).2) s

end B4.MM
