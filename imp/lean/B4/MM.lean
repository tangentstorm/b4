import B4.AsmSyntax
import B4.Theory
import B4.Heap

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

`MM.alloc` is the same algorithm on a list of blocks (`B4.Heap.alloc`, in
bytes: a header of 12, a split at 16), the model the code is meant to follow. For now they are related only by a random test (the
machine and the model agree); proving it is still to do.
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

/-- A block: the size of its data, in bytes, and whether it is used. -/
abbrev Blk := Heap.Blk

/-- Absorb the free blocks at the front of `bs` into a free block of `size`
(headers of 12 bytes). -/
abbrev absorb (size : Nat) (bs : List Blk) : Nat × List Blk := Heap.absorb 12 size bs

/-- Take a free block of `size` for `n`, splitting off the rest when it is at
least 16 bytes. -/
abbrev claim (n size : Nat) : List Blk := Heap.claim 12 16 n size

/-- **`mm-alloc`, on the blocks**: `Heap.alloc` in bytes, a header of 12 and a
split at 16 — the offset of the block taken from the start of the heap, if one
is big enough, and the blocks after, merged as far as the search went. -/
abbrev alloc (n : Nat) (bs : List Blk) : Option Nat × List Blk := Heap.alloc 12 16 n bs

/-- **`mm-free`, on the blocks**: the block at offset `o` is free. -/
abbrev free (o : Nat) (bs : List Blk) : List Blk := Heap.free 12 o bs

/-- **Bytes are four times cells**: the byte model on blocks four times the size
does what the model in cells (`Heap.alloc 3 4`, the allocator of Hehner's
language) does, with offsets four times. -/
theorem alloc_cells (n : Nat) (bs : List Blk) :
    alloc (4 * n) (bs.map (Heap.Blk.scale 4)) =
      ((Heap.alloc 3 4 n bs).1.map (4 * ·), (Heap.alloc 3 4 n bs).2.map (Heap.Blk.scale 4)) :=
  Heap.alloc_scale (k := 4) (by decide) 3 4 n bs

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
