import B4.Theory

/-!
# A swarm of b4 machines joined by channels

Several machines run side by side and talk over numbered channels. A channel is
a script of messages, each a value with the time it was sent; each machine
keeps, for each channel, how far it has read. A machine reaches its channels
through the `io` instruction, as b4 reaches any device:

* `v c 's' io` — send `v` on channel `c`, stamped with the machine's clock;
* `c 'r' io` — receive the next message on `c`: push its value, and move the
  clock to one past the time it was sent if that is later (a message takes one
  unit of time to arrive); if there is none yet, the machine waits.

The clock is register `T` (`getClk`). Any other `io` command, and every other
instruction, is the machine's own `step`; a machine on its own, outside a swarm,
treats `'s'` and `'r'` as it treats any unknown command: it does nothing.
-/

namespace B4

/-- A message: its value and the time it was sent. -/
abbrev Msg := UInt32 × UInt32

/-- Several machines, the channels' scripts, and each machine's read cursors. -/
structure Swarm where
  /-- The machines. -/
  ms : List State
  /-- The script of each channel. -/
  chans : Nat → List Msg
  /-- Each machine's read cursor on each channel. -/
  rd : List (Nat → Nat)

/-- `'s'`: send. -/
def SEND : UInt32 := 0x73
/-- `'r'`: receive. -/
def RECV : UInt32 := 0x72

/-- The `io` command a machine is about to run, if its next instruction is `io`. -/
def ioCmd (s : State) : Option UInt32 :=
  if s.mem.get! (getIP s) = 0xFD then (dstack s).getLast? else none

/-- Whether a machine is running. -/
def running (s : State) : Bool := getRST s == 1 && getRDB s == 0

/-- The later of two times. -/
def later (a b : UInt32) : UInt32 := if a.toNat < b.toNat then b else a

/-- Machine `i` sends: pop the command, the channel and the value. -/
def Swarm.send (w : Swarm) (i : Nat) (s : State) : Swarm :=
  let s₁ := (dpop s).2
  let c := (dpop s₁).1
  let s₂ := (dpop s₁).2
  let v := (dpop s₂).1
  let s₃ := (dpop s₂).2
  { w with ms := w.ms.set i (setIP s₃ (getIP s₃ + 1)),
           chans := fun c' => if c' = c.toNat then w.chans c' ++ [(v, getClk s₃)] else w.chans c' }

/-- Machine `i` receives, if the message is there. -/
def Swarm.recv (w : Swarm) (i : Nat) (s : State) : Option Swarm :=
  let s₁ := (dpop s).2
  let c := (dpop s₁).1
  let s₂ := (dpop s₁).2
  let r := (w.rd.getD i fun _ => 0) c.toNat
  match (w.chans c.toNat)[r]? with
  | none => none
  | some m =>
    let s₃ := dpush s₂ m.1
    let s₄ := setClk s₃ (later (getClk s₃) (m.2 + 1))
    some { w with ms := w.ms.set i (setIP s₄ (getIP s₄ + 1)),
                  rd := w.rd.set i fun c' => if c' = c.toNat then r + 1 else
                    (w.rd.getD i fun _ => 0) c' }

/-- **One step of machine `i`**, if it can take one: it must be running, and a
receive must find its message. -/
def Swarm.stepAt (w : Swarm) (i : Nat) : Option Swarm :=
  match w.ms[i]? with
  | none => none
  | some s =>
    if running s then
      if ioCmd s = some SEND then some (w.send i s)
      else if ioCmd s = some RECV then w.recv i s
      else some { w with ms := w.ms.set i (step s) }
    else none

/-- One round: each machine in turn takes a step if it can; and whether any did. -/
def Swarm.sweep (w : Swarm) : Swarm × Bool :=
  (List.range w.ms.length).foldl
    (fun acc i => match acc.1.stepAt i with
      | some w' => (w', true)
      | none => acc) (w, false)

/-- Run for at most `n` rounds, stopping when no machine can move. -/
def Swarm.run : Nat → Swarm → Swarm
  | 0, w => w
  | n + 1, w => if w.sweep.2 then Swarm.run n w.sweep.1 else w

/-- A step of some machine. -/
def Swarm.Step (w w' : Swarm) : Prop := ∃ i, w.stepAt i = some w'

/-- The swarm's runs. -/
inductive Swarm.Steps : Swarm → Swarm → Prop
  /-- No step. -/
  | refl {w : Swarm} : Swarm.Steps w w
  /-- One more step. -/
  | tail {w w' w'' : Swarm} : Swarm.Steps w w' → Swarm.Step w' w'' → Swarm.Steps w w''

theorem Swarm.Steps.trans {a b c : Swarm} (h₁ : Swarm.Steps a b) (h₂ : Swarm.Steps b c) :
    Swarm.Steps a c := by
  induction h₂ with
  | refl => exact h₁
  | tail _ hs ih => exact .tail ih hs

theorem Swarm.Steps.single {a b : Swarm} (h : Swarm.Step a b) : Swarm.Steps a b := .tail .refl h

/-! ### What a step is -/

theorem Swarm.stepAt_eq {w : Swarm} {i : Nat} {s : State} (h : w.ms[i]? = some s)
    (hr : running s = true) :
    w.stepAt i = if ioCmd s = some SEND then some (w.send i s)
      else if ioCmd s = some RECV then w.recv i s
      else some { w with ms := w.ms.set i (step s) } := by
  simp [Swarm.stepAt, h, hr]

/-- An instruction that is not a send or a receive is the machine's own step. -/
theorem Swarm.stepAt_other {w : Swarm} {i : Nat} {s : State} (h : w.ms[i]? = some s)
    (hr : running s = true) (hs : ioCmd s ≠ some SEND) (hv : ioCmd s ≠ some RECV) :
    w.stepAt i = some { w with ms := w.ms.set i (step s) } := by
  rw [Swarm.stepAt_eq h hr, ite_eq_right hs, ite_eq_right hv]

theorem Swarm.stepAt_send {w : Swarm} {i : Nat} {s : State} (h : w.ms[i]? = some s)
    (hr : running s = true) (hs : ioCmd s = some SEND) : w.stepAt i = some (w.send i s) := by
  rw [Swarm.stepAt_eq h hr, ite_eq_left hs]

theorem Swarm.stepAt_recv {w : Swarm} {i : Nat} {s : State} (h : w.ms[i]? = some s)
    (hr : running s = true) (hv : ioCmd s = some RECV) : w.stepAt i = w.recv i s := by
  rw [Swarm.stepAt_eq h hr, ite_eq_right (by rw [hv]; decide), ite_eq_left hv]

theorem running_iff (s : State) : running s = true ↔ Running s := by
  simp [running, Running]

/-! ### Loading -/

/-- A machine with `code` at `0x100`, running from there. -/
def loadCode (code : List UInt8) : State :=
  let m := (code.zipIdx).foldl (fun m (b, k) => m.set! (0x100 + k) b) mkInitialState.mem
  setRST (setIP { mkInitialState with mem := m } 0x100) 1

/-- A swarm of machines, no messages, nothing read. -/
def Swarm.ofMachines (ms : List State) : Swarm := ⟨ms, fun _ => [], ms.map fun _ _ => 0⟩

namespace SwarmDemo

/-- `2 0 's' io hl`: send `2` on channel `0`, and halt. -/
def sender : List UInt8 := (assemble [.li 2, .li 0, .li 0x73, .io, .hl]).toList

/-- `0 'r' io 0x200 wi hl`: receive on channel `0`, store at `0x200`, halt. -/
def receiver : List UInt8 := (assemble [.li 0, .li 0x72, .io, .li 0x200, .wi, .hl]).toList

/-- The two, run together. -/
def demo : Swarm := (Swarm.ofMachines [loadCode receiver, loadCode sender]).run 100

-- The receiver has `2` at `0x200`, and its clock is `1`: the message took a unit of time.
#eval (demo.ms.map fun s => (getVal s.mem 0x200, getClk s), demo.chans 0)

end SwarmDemo

end B4
