import B4.Basic

/-!
# The b4 machine, as a theory

Lemmas that let the machine of `B4.Basic` be reasoned about: a total, fuelled
`runN` beside the partial `run`; 32-bit cells of memory read back what was
written to them, and writing one leaves the others alone; and the stacks as
lists.
-/

namespace B4

/-! ### Bytes and cells -/

theorem get!_eq (a : ByteArray) (i : Nat) : a.get! i = a[i]! := by
  cases a with
  | mk bs =>
    simp only [ByteArray.get!]
    by_cases h : i < bs.size
    · rw [getElem!_pos (ByteArray.mk bs) i (by exact h), getElem!_pos bs i h]; rfl
    · rw [getElem!_neg (ByteArray.mk bs) i (by exact h), getElem!_neg bs i h]

@[simp] theorem size_set! (a : ByteArray) (i : Nat) (v : UInt8) : (a.set! i v).size = a.size :=
  ByteArray.size_set! a i v

theorem get!_set!_self (a : ByteArray) (i : Nat) (v : UInt8) (h : i < a.size) :
    (a.set! i v).get! i = v := by
  rw [get!_eq]; exact ByteArray.getElem!_set!_self a i v h

theorem get!_set!_ne (a : ByteArray) (i j : Nat) (v : UInt8) (h : i ≠ j) :
    (a.set! i v).get! j = a.get! j := by
  rw [get!_eq, get!_eq]; exact ByteArray.getElem!_set!_ne a i j v h

/-- Four bytes put back together are the word they came from. -/
theorem bytes_word (v : UInt32) :
    (v.toUInt8.toUInt32 ||| ((v >>> 8).toUInt8.toUInt32 <<< 8) |||
      ((v >>> 16).toUInt8.toUInt32 <<< 16) ||| ((v >>> 24).toUInt8.toUInt32 <<< 24)) = v := by
  bv_decide

@[simp] theorem size_setVal (m : ByteArray) (off : Nat) (v : UInt32) :
    (setVal m off v).size = m.size := by
  unfold setVal; split <;> simp

/-- Writing a cell leaves the bytes outside it alone. -/
theorem get!_setVal_of_lt (m : ByteArray) (off : Nat) (v : UInt32) (a : Nat)
    (h : a < off ∨ off + 4 ≤ a) : (setVal m off v).get! a = m.get! a := by
  unfold setVal
  split
  · rw [get!_set!_ne _ _ _ _ (by omega), get!_set!_ne _ _ _ _ (by omega),
      get!_set!_ne _ _ _ _ (by omega), get!_set!_ne _ _ _ _ (by omega)]
  · rfl

/-- A cell reads back what was written to it. -/
theorem getVal_setVal_self (m : ByteArray) (off : Nat) (v : UInt32) (h : off + 3 < m.size) :
    getVal (setVal m off v) off = v := by
  have hs : off + 3 < (setVal m off v).size := by simpa using h
  unfold getVal
  simp only [hs, ↓reduceIte]
  unfold setVal
  simp only [h, ↓reduceIte]
  rw [get!_set!_ne _ _ _ _ (by omega), get!_set!_ne _ _ _ _ (by omega),
    get!_set!_ne _ _ _ _ (by omega), get!_set!_self _ _ _ (by (try simp only [size_set!]); omega),
    get!_set!_ne _ _ _ _ (by omega), get!_set!_ne _ _ _ _ (by omega),
    get!_set!_self _ _ _ (by (try simp only [size_set!]); omega),
    get!_set!_ne _ _ _ _ (by omega), get!_set!_self _ _ _ (by (try simp only [size_set!]); omega),
    get!_set!_self _ _ _ (by (try simp only [size_set!]); omega)]
  exact bytes_word v

/-- Writing a cell leaves another, disjoint, cell alone. -/
theorem getVal_setVal_of_disjoint (m : ByteArray) (off off' : Nat) (v : UInt32)
    (h : off' + 4 ≤ off ∨ off + 4 ≤ off') : getVal (setVal m off v) off' = getVal m off' := by
  unfold getVal
  simp only [size_setVal]
  split
  · rw [get!_setVal_of_lt _ _ _ _ (by omega), get!_setVal_of_lt _ _ _ _ (by omega),
      get!_setVal_of_lt _ _ _ _ (by omega), get!_setVal_of_lt _ _ _ _ (by omega)]
  · rfl

/-! ### Registers -/

/-- The register cells, all in the first 256 bytes and pairwise apart. -/
theorem reg_offs : RIP_OFF + 4 ≤ RDS_OFF ∧ RDS_OFF + 4 ≤ RCS_OFF ∧ RCS_OFF + 4 ≤ RST_OFF ∧
    RST_OFF + 4 ≤ RDB_OFF ∧ RDB_OFF + 4 ≤ 256 := by decide

theorem toNat_toUInt32 {n : Nat} (h : n < 2 ^ 32) : n.toUInt32.toNat = n := by
  simp [Nat.toUInt32, Nat.mod_eq_of_lt h]

/-- A well-formed state: memory and stacks of their sizes, stack heights in range. -/
structure WF (s : State) : Prop where
  /-- The memory is 64 KB. -/
  mem : s.mem.size = MAXBYTE
  /-- The data stack has its size. -/
  ds : s.ds.size = STACKSZ
  /-- The control stack has its size. -/
  cs : s.cs.size = STACKSZ
  /-- The data stack's height is in range. -/
  dsh : getDSH s ≤ STACKSZ
  /-- The control stack's height is in range. -/
  csh : getCSH s ≤ STACKSZ

/-- The data stack, bottom first. -/
def dstack (s : State) : List UInt32 := s.ds.toList.take (getDSH s)

/-- The control stack, bottom first. -/
def cstack (s : State) : List UInt32 := s.cs.toList.take (getCSH s)

/-- The memory above the registers, where code and data live. -/
def high (s : State) (a : Nat) : UInt8 := if 256 ≤ a then s.mem.get! a else 0

/-- What a program sees of a state. -/
structure View where
  ip : Nat
  ds : List UInt32
  cs : List UInt32
  st : UInt32
  db : UInt32
  mem : Nat → UInt8
  ob : String

/-- The view of a state. -/
def view (s : State) : View :=
  ⟨getIP s, dstack s, cstack s, getRST s, getRDB s, high s, s.ob⟩

/-- Writing a register cell (`off + 4 ≤ 256`) changes nothing in high memory. -/
theorem high_setVal (s : State) (off : Nat) (v : UInt32) (h : off + 4 ≤ 256) :
    high { s with mem := setVal s.mem off v } = high s := by
  funext a
  unfold high
  split
  · exact get!_setVal_of_lt _ _ _ _ (by omega)
  · rfl


/-! ### Register access -/

@[simp] theorem getIP_setDSH (s : State) (v : Nat) : getIP (setDSH s v) = getIP s := by
  simp only [getIP, setDSH]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RIP_OFF RDS_OFF; omega)]

@[simp] theorem getIP_setCSH (s : State) (v : Nat) : getIP (setCSH s v) = getIP s := by
  simp only [getIP, setCSH]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RIP_OFF RCS_OFF; omega)]

@[simp] theorem getIP_setRST (s : State) (v : UInt32) : getIP (setRST s v) = getIP s := by
  simp only [getIP, setRST]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RIP_OFF RST_OFF; omega)]

@[simp] theorem getIP_setRDB (s : State) (v : UInt32) : getIP (setRDB s v) = getIP s := by
  simp only [getIP, setRDB]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RIP_OFF RDB_OFF; omega)]

@[simp] theorem getDSH_setIP (s : State) (v : Nat) : getDSH (setIP s v) = getDSH s := by
  simp only [getDSH, setIP]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RDS_OFF RIP_OFF; omega)]

@[simp] theorem getDSH_setCSH (s : State) (v : Nat) : getDSH (setCSH s v) = getDSH s := by
  simp only [getDSH, setCSH]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RDS_OFF RCS_OFF; omega)]

@[simp] theorem getDSH_setRST (s : State) (v : UInt32) : getDSH (setRST s v) = getDSH s := by
  simp only [getDSH, setRST]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RDS_OFF RST_OFF; omega)]

@[simp] theorem getDSH_setRDB (s : State) (v : UInt32) : getDSH (setRDB s v) = getDSH s := by
  simp only [getDSH, setRDB]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RDS_OFF RDB_OFF; omega)]

@[simp] theorem getCSH_setIP (s : State) (v : Nat) : getCSH (setIP s v) = getCSH s := by
  simp only [getCSH, setIP]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RCS_OFF RIP_OFF; omega)]

@[simp] theorem getCSH_setDSH (s : State) (v : Nat) : getCSH (setDSH s v) = getCSH s := by
  simp only [getCSH, setDSH]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RCS_OFF RDS_OFF; omega)]

@[simp] theorem getCSH_setRST (s : State) (v : UInt32) : getCSH (setRST s v) = getCSH s := by
  simp only [getCSH, setRST]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RCS_OFF RST_OFF; omega)]

@[simp] theorem getCSH_setRDB (s : State) (v : UInt32) : getCSH (setRDB s v) = getCSH s := by
  simp only [getCSH, setRDB]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RCS_OFF RDB_OFF; omega)]

@[simp] theorem getRST_setIP (s : State) (v : Nat) : getRST (setIP s v) = getRST s := by
  simp only [getRST, setIP]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RST_OFF RIP_OFF; omega)]

@[simp] theorem getRST_setDSH (s : State) (v : Nat) : getRST (setDSH s v) = getRST s := by
  simp only [getRST, setDSH]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RST_OFF RDS_OFF; omega)]

@[simp] theorem getRST_setCSH (s : State) (v : Nat) : getRST (setCSH s v) = getRST s := by
  simp only [getRST, setCSH]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RST_OFF RCS_OFF; omega)]

@[simp] theorem getRST_setRDB (s : State) (v : UInt32) : getRST (setRDB s v) = getRST s := by
  simp only [getRST, setRDB]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RST_OFF RDB_OFF; omega)]

@[simp] theorem getRDB_setIP (s : State) (v : Nat) : getRDB (setIP s v) = getRDB s := by
  simp only [getRDB, setIP]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RDB_OFF RIP_OFF; omega)]

@[simp] theorem getRDB_setDSH (s : State) (v : Nat) : getRDB (setDSH s v) = getRDB s := by
  simp only [getRDB, setDSH]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RDB_OFF RDS_OFF; omega)]

@[simp] theorem getRDB_setCSH (s : State) (v : Nat) : getRDB (setCSH s v) = getRDB s := by
  simp only [getRDB, setCSH]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RDB_OFF RCS_OFF; omega)]

@[simp] theorem getRDB_setRST (s : State) (v : UInt32) : getRDB (setRST s v) = getRDB s := by
  simp only [getRDB, setRST]
  rw [getVal_setVal_of_disjoint _ _ _ _ (by unfold RDB_OFF RST_OFF; omega)]

@[simp] theorem high_setIP (s : State) (v : Nat) : high (setIP s v) = high s :=
  high_setVal s _ _ (by unfold RIP_OFF; omega)

@[simp] theorem ob_setIP (s : State) (v : Nat) : (setIP s v).ob = s.ob := rfl

@[simp] theorem ds_setIP (s : State) (v : Nat) : (setIP s v).ds = s.ds := rfl

@[simp] theorem cs_setIP (s : State) (v : Nat) : (setIP s v).cs = s.cs := rfl

@[simp] theorem size_setIP (s : State) (v : Nat) : (setIP s v).mem.size = s.mem.size := by
  simp [setIP]

@[simp] theorem high_setDSH (s : State) (v : Nat) : high (setDSH s v) = high s :=
  high_setVal s _ _ (by unfold RDS_OFF; omega)

@[simp] theorem ob_setDSH (s : State) (v : Nat) : (setDSH s v).ob = s.ob := rfl

@[simp] theorem ds_setDSH (s : State) (v : Nat) : (setDSH s v).ds = s.ds := rfl

@[simp] theorem cs_setDSH (s : State) (v : Nat) : (setDSH s v).cs = s.cs := rfl

@[simp] theorem size_setDSH (s : State) (v : Nat) : (setDSH s v).mem.size = s.mem.size := by
  simp [setDSH]

@[simp] theorem high_setCSH (s : State) (v : Nat) : high (setCSH s v) = high s :=
  high_setVal s _ _ (by unfold RCS_OFF; omega)

@[simp] theorem ob_setCSH (s : State) (v : Nat) : (setCSH s v).ob = s.ob := rfl

@[simp] theorem ds_setCSH (s : State) (v : Nat) : (setCSH s v).ds = s.ds := rfl

@[simp] theorem cs_setCSH (s : State) (v : Nat) : (setCSH s v).cs = s.cs := rfl

@[simp] theorem size_setCSH (s : State) (v : Nat) : (setCSH s v).mem.size = s.mem.size := by
  simp [setCSH]

@[simp] theorem high_setRST (s : State) (v : UInt32) : high (setRST s v) = high s :=
  high_setVal s _ _ (by unfold RST_OFF; omega)

@[simp] theorem ob_setRST (s : State) (v : UInt32) : (setRST s v).ob = s.ob := rfl

@[simp] theorem ds_setRST (s : State) (v : UInt32) : (setRST s v).ds = s.ds := rfl

@[simp] theorem cs_setRST (s : State) (v : UInt32) : (setRST s v).cs = s.cs := rfl

@[simp] theorem size_setRST (s : State) (v : UInt32) : (setRST s v).mem.size = s.mem.size := by
  simp [setRST]

@[simp] theorem high_setRDB (s : State) (v : UInt32) : high (setRDB s v) = high s :=
  high_setVal s _ _ (by unfold RDB_OFF; omega)

@[simp] theorem ob_setRDB (s : State) (v : UInt32) : (setRDB s v).ob = s.ob := rfl

@[simp] theorem ds_setRDB (s : State) (v : UInt32) : (setRDB s v).ds = s.ds := rfl

@[simp] theorem cs_setRDB (s : State) (v : UInt32) : (setRDB s v).cs = s.cs := rfl

@[simp] theorem size_setRDB (s : State) (v : UInt32) : (setRDB s v).mem.size = s.mem.size := by
  simp [setRDB]

theorem getIP_setIP (s : State) (v : Nat) (h : v < 2 ^ 32) (hm : s.mem.size = MAXBYTE) :
    getIP (setIP s v) = v := by
  unfold getIP setIP
  rw [getVal_setVal_self _ _ _ (by rw [hm]; unfold RIP_OFF MAXBYTE; omega), toNat_toUInt32 h]

theorem getDSH_setDSH (s : State) (v : Nat) (h : v < 2 ^ 32) (hm : s.mem.size = MAXBYTE) :
    getDSH (setDSH s v) = v := by
  unfold getDSH setDSH
  rw [getVal_setVal_self _ _ _ (by rw [hm]; unfold RDS_OFF MAXBYTE; omega), toNat_toUInt32 h]

theorem getCSH_setCSH (s : State) (v : Nat) (h : v < 2 ^ 32) (hm : s.mem.size = MAXBYTE) :
    getCSH (setCSH s v) = v := by
  unfold getCSH setCSH
  rw [getVal_setVal_self _ _ _ (by rw [hm]; unfold RCS_OFF MAXBYTE; omega), toNat_toUInt32 h]

theorem getRST_setRST (s : State) (v : UInt32) (hm : s.mem.size = MAXBYTE) :
    getRST (setRST s v) = v := by
  unfold getRST setRST
  rw [getVal_setVal_self _ _ _ (by rw [hm]; unfold RST_OFF MAXBYTE; omega)]

theorem getRDB_setRDB (s : State) (v : UInt32) (hm : s.mem.size = MAXBYTE) :
    getRDB (setRDB s v) = v := by
  unfold getRDB setRDB
  rw [getVal_setVal_self _ _ _ (by rw [hm]; unfold RDB_OFF MAXBYTE; omega)]

/-! ### The stacks -/

theorem take_set_succ (a : Array UInt32) (h : Nat) (v : UInt32) (hh : h < a.size) :
    (a.set! h v).toList.take (h + 1) = a.toList.take h ++ [v] := by
  rw [Array.set!_eq_setIfInBounds, Array.toList_setIfInBounds, List.take_add_one]
  simp [List.take_set, hh]
  exact List.set_eq_of_length_le (by simp; omega)

theorem take_pred (a : Array UInt32) (xs : List UInt32) (v : UInt32) (h : Nat)
    (hs : h ≤ a.size) (hd : a.toList.take h = xs ++ [v]) :
    h = xs.length + 1 ∧ a.toList.take (h - 1) = xs ∧ a[h - 1]! = v := by
  have hl : (a.toList.take h).length = xs.length + 1 := by simp [hd]
  have hh : h = xs.length + 1 := by simp at hl; omega
  subst hh
  refine ⟨rfl, ?_, ?_⟩
  · have := congrArg (List.take xs.length) hd
    simpa [List.take_take] using this
  · have := congrArg (·[xs.length]?) hd
    simp at this
    rw [Nat.add_sub_cancel, getElem!_pos a xs.length (by omega)]
    rw [Array.getElem?_eq_getElem (by omega)] at this
    exact Option.some.inj this

theorem dpush_view (s : State) (v : UInt32) (hw : WF s) (h : getDSH s < STACKSZ) :
    WF (dpush s v) ∧ getIP (dpush s v) = getIP s ∧ dstack (dpush s v) = dstack s ++ [v] ∧
      cstack (dpush s v) = cstack s ∧ getRST (dpush s v) = getRST s ∧
      getRDB (dpush s v) = getRDB s ∧ high (dpush s v) = high s ∧ (dpush s v).ob = s.ob := by
  unfold dpush
  simp only [h, ↓reduceIte]
  have hm := hw.mem
  have hdsh : getDSH (setDSH { s with ds := s.ds.set! (getDSH s) v } (getDSH s + 1)) =
      getDSH s + 1 := getDSH_setDSH _ _ (by unfold STACKSZ at h; omega) (by simpa using hm)
  refine ⟨⟨by simpa using hm, by simpa using hw.ds, by simpa using hw.cs, by omega,
    by rw [getCSH_setDSH]; exact hw.csh⟩, by simp; rfl, ?_, by simp [cstack]; rfl, by simp; rfl, by simp; rfl,
    by simp; rfl, rfl⟩
  unfold dstack
  rw [hdsh]
  simp only [ds_setDSH]
  exact take_set_succ _ _ _ (by rw [hw.ds]; exact h)

theorem dpop_view (s : State) (xs : List UInt32) (v : UInt32) (hw : WF s)
    (hd : dstack s = xs ++ [v]) :
    (dpop s).1 = v ∧ WF (dpop s).2 ∧ getIP (dpop s).2 = getIP s ∧ dstack (dpop s).2 = xs ∧
      cstack (dpop s).2 = cstack s ∧ getRST (dpop s).2 = getRST s ∧
      getRDB (dpop s).2 = getRDB s ∧ high (dpop s).2 = high s ∧ (dpop s).2.ob = s.ob := by
  obtain ⟨hh, ht, hv⟩ := take_pred s.ds xs v (getDSH s) (by rw [hw.ds]; exact hw.dsh) hd
  unfold dpop
  have hpos : getDSH s > 0 := by omega
  simp only [hpos, ↓reduceIte]
  have hm := hw.mem
  have hdsh : getDSH (setDSH s (getDSH s - 1)) = xs.length :=
    by rw [getDSH_setDSH _ _ (by have := hw.dsh; unfold STACKSZ at this; omega) hm]; omega
  refine ⟨hv, ⟨by simpa using hm, by simpa using hw.ds, by simpa using hw.cs,
    by rw [hdsh]; have := hw.dsh; omega, by rw [getCSH_setDSH]; exact hw.csh⟩, by simp, ?_, by simp [cstack],
    by simp, by simp, by simp, rfl⟩
  · unfold dstack; rw [hdsh, ds_setDSH]; rw [hh] at ht; simpa using ht

theorem cpush_view (s : State) (v : UInt32) (hw : WF s) (h : getCSH s < STACKSZ) :
    WF (cpush s v) ∧ getIP (cpush s v) = getIP s ∧ dstack (cpush s v) = dstack s ∧
      cstack (cpush s v) = cstack s ++ [v] ∧ getRST (cpush s v) = getRST s ∧
      getRDB (cpush s v) = getRDB s ∧ high (cpush s v) = high s ∧ (cpush s v).ob = s.ob := by
  unfold cpush
  simp only [h, ↓reduceIte]
  have hm := hw.mem
  have hcsh : getCSH (setCSH { s with cs := s.cs.set! (getCSH s) v } (getCSH s + 1)) =
      getCSH s + 1 := getCSH_setCSH _ _ (by unfold STACKSZ at h; omega) (by simpa using hm)
  refine ⟨⟨by simpa using hm, by simpa using hw.ds, by simpa using hw.cs,
    by rw [getDSH_setCSH]; exact hw.dsh, by omega⟩, by simp; rfl, by simp [dstack]; rfl, ?_,
    by simp; rfl, by simp; rfl, by simp; rfl, rfl⟩
  unfold cstack
  rw [hcsh]
  simp only [cs_setCSH]
  exact take_set_succ _ _ _ (by rw [hw.cs]; exact h)

theorem cpop_view (s : State) (xs : List UInt32) (v : UInt32) (hw : WF s)
    (hd : cstack s = xs ++ [v]) :
    (cpop s).1 = v ∧ WF (cpop s).2 ∧ getIP (cpop s).2 = getIP s ∧ dstack (cpop s).2 = dstack s ∧
      cstack (cpop s).2 = xs ∧ getRST (cpop s).2 = getRST s ∧
      getRDB (cpop s).2 = getRDB s ∧ high (cpop s).2 = high s ∧ (cpop s).2.ob = s.ob := by
  obtain ⟨hh, ht, hv⟩ := take_pred s.cs xs v (getCSH s) (by rw [hw.cs]; exact hw.csh) hd
  unfold cpop
  have hpos : getCSH s > 0 := by omega
  simp only [hpos, ↓reduceIte]
  have hm := hw.mem
  have hcsh : getCSH (setCSH s (getCSH s - 1)) = xs.length :=
    by rw [getCSH_setCSH _ _ (by have := hw.csh; unfold STACKSZ at this; omega) hm]; omega
  refine ⟨hv, ⟨by simpa using hm, by simpa using hw.ds, by simpa using hw.cs,
    by rw [getDSH_setCSH]; exact hw.dsh, by rw [hcsh]; have := hw.csh; omega⟩, by simp, by simp [dstack], ?_,
    by simp, by simp, by simp, rfl⟩
  · unfold cstack; rw [hcsh, cs_setCSH]; rw [hh] at ht; simpa using ht

theorem setIP_view (s : State) (n : Nat) (hw : WF s) (hn : n < 2 ^ 32) :
    WF (setIP s n) ∧ getIP (setIP s n) = n ∧ dstack (setIP s n) = dstack s ∧
      cstack (setIP s n) = cstack s ∧ getRST (setIP s n) = getRST s ∧
      getRDB (setIP s n) = getRDB s ∧ high (setIP s n) = high s ∧ (setIP s n).ob = s.ob :=
  ⟨⟨by simpa using hw.mem, hw.ds, hw.cs, by rw [getDSH_setIP]; exact hw.dsh,
    by rw [getCSH_setIP]; exact hw.csh⟩, getIP_setIP _ _ hn hw.mem, by simp [dstack],
    by simp [cstack], by simp, by simp, by simp, rfl⟩

/-! ### Running -/

/-- `run`, with fuel: at most `n` steps. -/
def runN : Nat → State → State
  | 0, s => s
  | n + 1, s => if getRST s == 1 && getRDB s == 0 then runN n (step s) else s

/-- The machine is running: `ST = 1` and not stopped for the debugger. -/
def Running (s : State) : Prop := getRST s = 1 ∧ getRDB s = 0

theorem runN_succ_of_running (n : Nat) (s : State) (h : Running s) :
    runN (n + 1) s = runN n (step s) := by
  simp [runN, h.1, h.2]

/-- The byte at the instruction pointer, in high memory. -/
theorem get!_ip (s : State) (h : 256 ≤ getIP s) : s.mem.get! (getIP s) = high s (getIP s) := by
  simp [high, h]

theorem dstack_length (s : State) (hw : WF s) : (dstack s).length = getDSH s := by
  have := hw.dsh; simp [dstack, hw.ds]; omega

theorem cstack_length (s : State) (hw : WF s) : (cstack s).length = getCSH s := by
  have := hw.csh; simp [cstack, hw.cs]; omega

/-- A step is the instruction at the pointer, then the pointer moves on one. -/
theorem step_of (s : State) (op : UInt8) (h : 256 ≤ getIP s) (hop : high s (getIP s) = op) :
    step s = setIP (runOp s op) (getIP (runOp s op) + 1) := by
  unfold step
  simp only
  rw [get!_ip s h, hop]

/-- Two states that agree on everything but the instruction pointer and the data stack. -/
structure Same (s s' : State) : Prop where
  cs : cstack s' = cstack s
  st : getRST s' = getRST s
  db : getRDB s' = getRDB s
  high : high s' = high s
  ob : s'.ob = s.ob

theorem Same.refl (s : State) : Same s s := ⟨rfl, rfl, rfl, rfl, rfl⟩

theorem Same.trans {s₁ s₂ s₃ : State} (h₁ : Same s₁ s₂) (h₂ : Same s₂ s₃) : Same s₁ s₃ :=
  ⟨h₂.cs.trans h₁.cs, h₂.st.trans h₁.st, h₂.db.trans h₁.db, h₂.high.trans h₁.high,
    h₂.ob.trans h₁.ob⟩

theorem dpush_same (s : State) (v : UInt32) (hw : WF s) (h : getDSH s < STACKSZ) :
    WF (dpush s v) ∧ getIP (dpush s v) = getIP s ∧ dstack (dpush s v) = dstack s ++ [v] ∧
      Same s (dpush s v) := by
  obtain ⟨w, i, d, c, r, b, hh, o⟩ := dpush_view s v hw h
  exact ⟨w, i, d, c, r, b, hh, o⟩

theorem dpop_same (s : State) (xs : List UInt32) (v : UInt32) (hw : WF s)
    (hd : dstack s = xs ++ [v]) :
    (dpop s).1 = v ∧ WF (dpop s).2 ∧ getIP (dpop s).2 = getIP s ∧ dstack (dpop s).2 = xs ∧
      Same s (dpop s).2 := by
  obtain ⟨e, w, i, d, c, r, b, hh, o⟩ := dpop_view s xs v hw hd
  exact ⟨e, w, i, d, c, r, b, hh, o⟩

theorem setIP_same (s : State) (n : Nat) (hw : WF s) (hn : n < 2 ^ 32) :
    WF (setIP s n) ∧ getIP (setIP s n) = n ∧ dstack (setIP s n) = dstack s ∧ Same s (setIP s n) := by
  obtain ⟨w, i, d, c, r, b, hh, o⟩ := setIP_view s n hw hn
  exact ⟨w, i, d, c, r, b, hh, o⟩

/-- An instruction that leaves the pointer alone, then moves on one. -/
theorem step_next (s : State) (op : UInt8) (ds : List UInt32) (hip : 256 ≤ getIP s)
    (hlt : getIP s + 1 < 2 ^ 32) (hop : high s (getIP s) = op)
    (hr : WF (runOp s op) ∧ getIP (runOp s op) = getIP s ∧ dstack (runOp s op) = ds ∧
      Same s (runOp s op)) :
    WF (step s) ∧ getIP (step s) = getIP s + 1 ∧ dstack (step s) = ds ∧ Same s (step s) := by
  rw [step_of s op hip hop]
  obtain ⟨w, i, d, sm⟩ := hr
  obtain ⟨w', i', d', sm'⟩ := setIP_same _ (getIP (runOp s op) + 1) w (by omega)
  exact ⟨w', by rw [i', i], by rw [d', d], sm.trans sm'⟩

/-- A binary operation on the top two items of the data stack. -/
theorem binop_same (s : State) (xs : List UInt32) (x y : UInt32) (f : UInt32 → UInt32 → UInt32)
    (hw : WF s) (hd : dstack s = xs ++ [x, y]) :
    let r := (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (f x y))
    WF r ∧ getIP r = getIP s ∧ dstack r = xs ++ [f x y] ∧ Same s r := by
  obtain ⟨e1, w1, i1, d1, sm1⟩ := dpop_same s (xs ++ [x]) y hw (by simpa using hd)
  obtain ⟨e2, w2, i2, d2, sm2⟩ := dpop_same _ xs x w1 d1
  have hl : getDSH (dpop (dpop s).2).2 < STACKSZ := by
    have h1 := dstack_length _ w2; have h2 := dstack_length s hw; have h3 := hw.dsh
    rw [d2] at h1; rw [hd] at h2; simp at h2; omega
  obtain ⟨w3, i3, d3, sm3⟩ := dpush_same _ (f x y) w2 hl
  simp only
  rw [e1, e2]
  exact ⟨w3, by rw [i3, i2, i1], by rw [d3, d2], sm1.trans (sm2.trans sm3)⟩

/-! ### Instructions -/

theorem step_binop (s : State) (op : UInt8) (f : UInt32 → UInt32 → UInt32) (xs : List UInt32)
    (x y : UInt32) (hw : WF s) (hip : 256 ≤ getIP s) (hlt : getIP s + 1 < 2 ^ 32)
    (hop : high s (getIP s) = op) (hd : dstack s = xs ++ [x, y])
    (hr : runOp s op = (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (f x y))) :
    WF (step s) ∧ getIP (step s) = getIP s + 1 ∧ dstack (step s) = xs ++ [f x y] ∧
      Same s (step s) :=
  step_next s op _ hip hlt hop (hr ▸ binop_same s xs x y f hw hd)

theorem runOp_ad (s : State) : runOp s 0x80 =
    (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (fromInt32 (toInt32 x + toInt32 y))) := by
  simp [runOp]


theorem runOp_sb (s : State) : runOp s 0x81 =
    (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (fromInt32 (toInt32 x - toInt32 y))) := by
  simp [runOp]

theorem runOp_ml (s : State) : runOp s 0x82 =
    (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (fromInt32 (toInt32 x * toInt32 y))) := by
  simp [runOp]

theorem runOp_an (s : State) : runOp s 0x86 =
    (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (x &&& y)) := by
  simp [runOp]

theorem runOp_or (s : State) : runOp s 0x87 =
    (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (x ||| y)) := by
  simp [runOp]

theorem runOp_xr (s : State) : runOp s 0x88 =
    (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (x ^^^ y)) := by
  simp [runOp]

theorem runOp_eq (s : State) : runOp s 0x8A =
    (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (if x == y then 0xFFFFFFFF else 0)) := by
  simp [runOp]

theorem runOp_lt (s : State) : runOp s 0x8B =
    (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (if toInt32 x < toInt32 y then 0xFFFFFFFF else 0)) := by
  simp [runOp]

/-- The word at `a` of a memory given byte by byte. -/
def word (m : Nat → UInt8) (a : Nat) : UInt32 :=
  (m a).toUInt32 ||| ((m (a + 1)).toUInt32 <<< 8) ||| ((m (a + 2)).toUInt32 <<< 16) |||
    ((m (a + 3)).toUInt32 <<< 24)

/-- A word of high memory. -/
theorem getVal_high (s : State) (a : Nat) (hw : WF s) (ha : 256 ≤ a) (hb : a + 3 < MAXBYTE) :
    getVal s.mem a = word (high s) a := by
  unfold getVal word high
  rw [hw.mem]
  simp only [hb, ↓reduceIte, show 256 ≤ a from ha, show 256 ≤ a + 1 by omega,
    show 256 ≤ a + 2 by omega, show 256 ≤ a + 3 by omega]

theorem runOp_li (s : State) : runOp s 0x97 =
    (setIP (dpush s (getVal s.mem (getIP s + 1))) (getIP s + 4)) := by
  simp [runOp]

/-- `li v`: push the word after the instruction, and skip it. -/
theorem step_li (s : State) (xs : List UInt32) (hw : WF s) (hip : 256 ≤ getIP s)
    (hlt : getIP s + 5 < MAXBYTE) (hop : high s (getIP s) = 0x97) (hd : dstack s = xs)
    (hfit : xs.length < STACKSZ) :
    WF (step s) ∧ getIP (step s) = getIP s + 5 ∧
      dstack (step s) = xs ++ [word (high s) (getIP s + 1)] ∧ Same s (step s) := by
  rw [step_of s _ hip hop, runOp_li]
  have hl : getDSH s < STACKSZ := by rw [← dstack_length s hw, hd]; exact hfit
  obtain ⟨w1, i1, d1, sm1⟩ := dpush_same s (getVal s.mem (getIP s + 1)) hw hl
  obtain ⟨w2, i2, d2, sm2⟩ := setIP_same _ (getIP s + 4) w1 (by unfold MAXBYTE at hlt; omega)
  obtain ⟨w3, i3, d3, sm3⟩ := setIP_same _ (getIP (setIP (dpush s (getVal s.mem (getIP s + 1)))
    (getIP s + 4)) + 1) w2 (by rw [i2]; unfold MAXBYTE at hlt; omega)
  refine ⟨w3, by rw [i3, i2], by rw [d3, d2, d1, hd, getVal_high s _ hw (by omega) (by omega)],
    sm1.trans (sm2.trans sm3)⟩



/-- `step_binop` for each binary instruction: its operation. -/
theorem step_binop' (s : State) (op : UInt8) (f : UInt32 → UInt32 → UInt32) (xs : List UInt32)
    (x y : UInt32) (hw : WF s) (hip : 256 ≤ getIP s) (hlt : getIP s + 1 < 2 ^ 32)
    (hop : high s (getIP s) = op) (hd : dstack s = xs ++ [x, y])
    (hr : ∀ s, runOp s op = (let (y, s) := dpop s; let (x, s) := dpop s; dpush s (f x y))) :
    WF (step s) ∧ getIP (step s) = getIP s + 1 ∧ dstack (step s) = xs ++ [f x y] ∧
      Same s (step s) :=
  step_binop s op f xs x y hw hip hlt hop hd (hr s)

theorem runOp_nt (s : State) : runOp s 0x89 =
    (let (x, s) := dpop s; dpush s (x ^^^ 0xFFFFFFFF)) := by
  simp [runOp]

/-- `nt`: complement the top of the stack. -/
theorem step_nt (s : State) (xs : List UInt32) (x : UInt32) (hw : WF s) (hip : 256 ≤ getIP s)
    (hlt : getIP s + 1 < 2 ^ 32) (hop : high s (getIP s) = 0x89) (hd : dstack s = xs ++ [x]) :
    WF (step s) ∧ getIP (step s) = getIP s + 1 ∧ dstack (step s) = xs ++ [x ^^^ 0xFFFFFFFF] ∧
      Same s (step s) := by
  refine step_next s _ _ hip hlt hop ?_
  rw [runOp_nt]
  obtain ⟨e1, w1, i1, d1, sm1⟩ := dpop_same s xs x hw hd
  have hl : getDSH (dpop s).2 < STACKSZ := by
    have h1 := dstack_length _ w1; have h2 := dstack_length s hw; have h3 := hw.dsh
    rw [d1] at h1; rw [hd] at h2; simp at h2; omega
  obtain ⟨w2, i2, d2, sm2⟩ := dpush_same _ (x ^^^ 0xFFFFFFFF) w1 hl
  simp only
  rw [e1]
  exact ⟨w2, by rw [i2, i1], by rw [d2, d1], sm1.trans sm2⟩

theorem runOp_ri (s : State) : runOp s 0x93 =
    (let (a, s) := dpop s; dpush s (getVal s.mem a.toNat)) := by
  simp [runOp]

/-- `ri`: replace an address on the stack by the word there. -/
theorem step_ri (s : State) (xs : List UInt32) (a : UInt32) (hw : WF s) (hip : 256 ≤ getIP s)
    (hlt : getIP s + 1 < 2 ^ 32) (hop : high s (getIP s) = 0x93) (hd : dstack s = xs ++ [a])
    (ha : 256 ≤ a.toNat) (hb : a.toNat + 3 < MAXBYTE) :
    WF (step s) ∧ getIP (step s) = getIP s + 1 ∧
      dstack (step s) = xs ++ [word (high s) a.toNat] ∧ Same s (step s) := by
  refine step_next s _ _ hip hlt hop ?_
  rw [runOp_ri]
  obtain ⟨e1, w1, i1, d1, sm1⟩ := dpop_same s xs a hw hd
  have hl : getDSH (dpop s).2 < STACKSZ := by
    have h1 := dstack_length _ w1; have h2 := dstack_length s hw; have h3 := hw.dsh
    rw [d1] at h1; rw [hd] at h2; simp at h2; omega
  obtain ⟨w2, i2, d2, sm2⟩ := dpush_same _ (getVal (dpop s).2.mem a.toNat) w1 hl
  simp only
  rw [e1]
  refine ⟨w2, by rw [i2, i1], ?_, sm1.trans sm2⟩
  rw [d2, d1, getVal_high _ _ w1 ha hb, sm1.high]

/-- High memory after a word is written at `a`. -/
def writeWord (m : Nat → UInt8) (a : Nat) (v : UInt32) : Nat → UInt8 := fun i =>
  if i = a then v.toUInt8 else if i = a + 1 then (v >>> 8).toUInt8
  else if i = a + 2 then (v >>> 16).toUInt8 else if i = a + 3 then (v >>> 24).toUInt8 else m i

theorem get!_setVal (m : ByteArray) (a : Nat) (v : UInt32) (hs : a + 3 < m.size) (i : Nat) :
    (setVal m a v).get! i = writeWord m.get! a v i := by
  unfold setVal writeWord
  simp only [hs, ↓reduceIte]
  by_cases h0 : i = a
  · subst h0
    rw [get!_set!_ne _ _ _ _ (by omega), get!_set!_ne _ _ _ _ (by omega),
      get!_set!_ne _ _ _ _ (by omega), get!_set!_self _ _ _ (by omega)]; simp
  by_cases h1 : i = a + 1
  · subst h1
    rw [get!_set!_ne _ _ _ _ (by omega), get!_set!_ne _ _ _ _ (by omega),
      get!_set!_self _ _ _ (by simp; omega)]; simp
  by_cases h2 : i = a + 2
  · subst h2
    rw [get!_set!_ne _ _ _ _ (by omega), get!_set!_self _ _ _ (by simp; omega)]; simp
  by_cases h3 : i = a + 3
  · subst h3
    rw [get!_set!_self _ _ _ (by simp; omega)]; simp
  rw [get!_set!_ne _ _ _ _ (by omega), get!_set!_ne _ _ _ _ (by omega),
    get!_set!_ne _ _ _ _ (by omega), get!_set!_ne _ _ _ _ (by omega)]
  simp [h0, h1, h2, h3]

theorem high_setVal_high (s : State) (a : Nat) (v : UInt32) (hw : WF s) (ha : 256 ≤ a)
    (hb : a + 3 < MAXBYTE) :
    high { s with mem := setVal s.mem a v } = writeWord (high s) a v := by
  funext i
  have hs : a + 3 < s.mem.size := by rw [hw.mem]; exact hb
  unfold high
  simp only
  rw [get!_setVal _ _ _ hs]
  unfold writeWord
  by_cases hi : 256 ≤ i
  · simp [hi]
  · simp [hi, show i ≠ a by omega, show i ≠ a + 1 by omega, show i ≠ a + 2 by omega,
      show i ≠ a + 3 by omega]

theorem word_writeWord_self (m : Nat → UInt8) (a : Nat) (v : UInt32) : word (writeWord m a v) a = v := by
  simp [word, writeWord]
  exact bytes_word v

theorem word_writeWord_of_disjoint (m : Nat → UInt8) (a b : Nat) (v : UInt32)
    (h : a + 4 ≤ b ∨ b + 4 ≤ a) : word (writeWord m a v) b = word m b := by
  unfold word writeWord
  simp only [show b ≠ a by omega, show b ≠ a + 1 by omega, show b ≠ a + 2 by omega,
    show b ≠ a + 3 by omega, show b + 1 ≠ a by omega, show b + 1 ≠ a + 1 by omega,
    show b + 1 ≠ a + 2 by omega, show b + 1 ≠ a + 3 by omega, show b + 2 ≠ a by omega,
    show b + 2 ≠ a + 1 by omega, show b + 2 ≠ a + 2 by omega, show b + 2 ≠ a + 3 by omega,
    show b + 3 ≠ a by omega, show b + 3 ≠ a + 1 by omega, show b + 3 ≠ a + 2 by omega,
    show b + 3 ≠ a + 3 by omega, ↓reduceIte]


theorem runOp_wi (s : State) : runOp s 0x95 =
    (let (a, s) := dpop s; let (v, s) := dpop s; { s with mem := setVal s.mem a.toNat v }) := by
  simp [runOp]

/-- Writing high memory leaves the registers and the stacks alone. -/
theorem setVal_high_regs (s : State) (a : Nat) (v : UInt32) (ha : 256 ≤ a) :
    let s' := { s with mem := setVal s.mem a v }
    getIP s' = getIP s ∧ getDSH s' = getDSH s ∧ getCSH s' = getCSH s ∧ getRST s' = getRST s ∧
      getRDB s' = getRDB s := by
  simp only [getIP, getDSH, getCSH, getRST, getRDB]
  refine ⟨?_, ?_, ?_, ?_, ?_⟩ <;>
    rw [getVal_setVal_of_disjoint _ _ _ _ (by simp only [RIP_OFF, RDS_OFF, RCS_OFF, RST_OFF, RDB_OFF]; omega)]

/-- `wi`: store the second item at the address on top. -/
theorem step_wi (s : State) (xs : List UInt32) (v a : UInt32) (hw : WF s) (hip : 256 ≤ getIP s)
    (hlt : getIP s + 1 < 2 ^ 32) (hop : high s (getIP s) = 0x95) (hd : dstack s = xs ++ [v, a])
    (ha : 256 ≤ a.toNat) (hb : a.toNat + 3 < MAXBYTE) :
    WF (step s) ∧ getIP (step s) = getIP s + 1 ∧ dstack (step s) = xs ∧
      cstack (step s) = cstack s ∧ getRST (step s) = getRST s ∧ getRDB (step s) = getRDB s ∧
      high (step s) = writeWord (high s) a.toNat v ∧ (step s).ob = s.ob := by
  rw [step_of s _ hip hop, runOp_wi]
  obtain ⟨e1, w1, i1, d1, sm1⟩ := dpop_same s (xs ++ [v]) a hw (by simpa using hd)
  obtain ⟨e2, w2, i2, d2, sm2⟩ := dpop_same _ xs v w1 d1
  simp only
  rw [e1, e2]
  generalize hs2 : (dpop (dpop s).2).2 = s2 at *
  obtain ⟨r1, r2, r3, r4, r5⟩ := setVal_high_regs s2 a.toNat v ha
  have w3 : WF { s2 with mem := setVal s2.mem a.toNat v } :=
    ⟨by simpa using w2.mem, w2.ds, w2.cs, by rw [r2]; exact w2.dsh, by rw [r3]; exact w2.csh⟩
  obtain ⟨w4, i4, d4, sm4⟩ := setIP_same _ (getIP { s2 with mem := setVal s2.mem a.toNat v } + 1)
    w3 (by rw [r1]; omega)
  have hsm := sm1.trans sm2
  refine ⟨w4, by rw [i4, r1, i2, i1], by rw [d4, dstack, r2]; exact d2, ?_, ?_, ?_, ?_, ?_⟩
  · rw [sm4.cs, cstack, r3]; exact hsm.cs
  · rw [sm4.st, r4]; exact hsm.st
  · rw [sm4.db, r5]; exact hsm.db
  · rw [sm4.high, high_setVal_high _ _ _ w2 ha hb, hsm.high]
  · rw [sm4.ob]; exact hsm.ob

/-- `go`: a jump into high memory lands one before its target, for the step's move on. -/
theorem go_eq (s : State) (a : Nat) (ha : 256 ≤ a) : go s a = setIP s (a - 1) := by
  unfold go; simp [show ¬ a < 0x100 by omega]

theorem runOp_jm (s : State) : runOp s 0x9A = go s (getVal s.mem (getIP s + 1)).toNat := by
  simp [runOp]

/-- `jm a`: jump to the address after the instruction. -/
theorem step_jm (s : State) (hw : WF s) (hip : 256 ≤ getIP s) (hlt : getIP s + 5 < MAXBYTE)
    (hop : high s (getIP s) = 0x9A) (ha : 256 ≤ (word (high s) (getIP s + 1)).toNat) :
    WF (step s) ∧ getIP (step s) = (word (high s) (getIP s + 1)).toNat ∧
      dstack (step s) = dstack s ∧ Same s (step s) := by
  rw [step_of s _ hip hop, runOp_jm, getVal_high s _ hw (by omega) (by omega), go_eq _ _ ha]
  have hlt32 : (word (high s) (getIP s + 1)).toNat < 2 ^ 32 := UInt32.toNat_lt _
  obtain ⟨w1, i1, d1, sm1⟩ := setIP_same s ((word (high s) (getIP s + 1)).toNat - 1) hw (by omega)
  obtain ⟨w2, i2, d2, sm2⟩ := setIP_same _ (getIP (setIP s ((word (high s) (getIP s + 1)).toNat - 1)) + 1)
    w1 (by rw [i1]; omega)
  exact ⟨w2, by rw [i2, i1]; omega, by rw [d2, d1], sm1.trans sm2⟩


/-- A byte as a signed distance. -/
def sbyte (b : UInt8) : Int := if b >= 128 then (b.toNat : Int) - 256 else (b.toNat : Int)

theorem runOp_h0 (s : State) : runOp s 0x9C =
    (let (v, s) := dpop s; if v == 0 then hop s else setIP s (getIP s + 1)) := by
  simp [runOp]

/-- `h0 d`: pop; if it was zero, hop by `d` from this instruction, else go past it. -/
theorem step_h0 (s : State) (xs : List UInt32) (v : UInt32) (hw : WF s) (hip : 256 ≤ getIP s)
    (hlt : getIP s + 2 < MAXBYTE) (hop : high s (getIP s) = 0x9C) (hd : dstack s = xs ++ [v])
    (ht : 256 ≤ (Int.ofNat (getIP s) + sbyte (high s (getIP s + 1))).toNat)
    (ht' : (Int.ofNat (getIP s) + sbyte (high s (getIP s + 1))).toNat < MAXBYTE) :
    WF (step s) ∧
      getIP (step s) = (if v = 0 then (Int.ofNat (getIP s) + sbyte (high s (getIP s + 1))).toNat
        else getIP s + 2) ∧
      dstack (step s) = xs ∧ Same s (step s) := by
  rw [step_of s _ hip hop, runOp_h0]
  obtain ⟨e1, w1, i1, d1, sm1⟩ := dpop_same s xs v hw hd
  simp only
  rw [e1]
  generalize hs1 : (dpop s).2 = s1 at *
  have hb : s1.mem.get! (getIP s1 + 1) = high s (getIP s + 1) := by
    rw [i1, ← sm1.high]; simp [high]; omega
  by_cases hv : v = 0
  · subst hv
    simp only [beq_self_eq_true, ↓reduceIte]
    unfold B4.hop
    simp only
    rw [hb]
    have hsb : (if high s (getIP s + 1) ≥ 128 then ((high s (getIP s + 1)).toNat : Int) - 256
        else ((high s (getIP s + 1)).toNat : Int)) = sbyte (high s (getIP s + 1)) := rfl
    rw [hsb, i1, go_eq _ _ ht]
    obtain ⟨w2, i2, d2, sm2⟩ := setIP_same s1 ((Int.ofNat (getIP s) + sbyte (high s (getIP s + 1))).toNat - 1)
      w1 (by unfold MAXBYTE at ht'; omega)
    obtain ⟨w3, i3, d3, sm3⟩ := setIP_same _ (getIP (setIP s1
      ((Int.ofNat (getIP s) + sbyte (high s (getIP s + 1))).toNat - 1)) + 1) w2
      (by rw [i2]; unfold MAXBYTE at ht'; omega)
    exact ⟨w3, by rw [i3, i2]; omega, by rw [d3, d2, d1], sm1.trans (sm2.trans sm3)⟩
  · have : (v == 0) = false := by simpa using hv
    simp only [this, Bool.false_eq_true, ↓reduceIte]
    obtain ⟨w2, i2, d2, sm2⟩ := setIP_same s1 (getIP s1 + 1) w1 (by rw [i1]; unfold MAXBYTE at hlt; omega)
    obtain ⟨w3, i3, d3, sm3⟩ := setIP_same _ (getIP (setIP s1 (getIP s1 + 1)) + 1) w2
      (by rw [i2, i1]; unfold MAXBYTE at hlt; omega)
    exact ⟨w3, by rw [i3, i2, i1]; simp [hv], by rw [d3, d2, d1], sm1.trans (sm2.trans sm3)⟩

theorem runOp_hl (s : State) : runOp s 0xFF = setRST s 0 := by
  simp [runOp]

/-- `hl`: halt — the machine stops running. -/
theorem step_hl (s : State) (hw : WF s) (hip : 256 ≤ getIP s) (hop : high s (getIP s) = 0xFF) :
    getRST (step s) = 0 := by
  rw [step_of s _ hip hop, runOp_hl, getRST_setIP, getRST_setRST _ _ hw.mem]

end B4
