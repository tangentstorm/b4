import B4
open B4

/-! b4a written inline in Lean, assembled when this file is compiled. -/

/-- `rn` in plain b4a (as `b4a/rn.b4a`), and a `main` that draws twice. -/
def rnd : Asm.Program := b4a! r#"
:rnd
  li B9 79 37 9E lb B4 ri ad      # (-z) z = seed + golden-ratio step
  du lb B4 wi                     # (z-z) seed := z
  du ls F0 sh xr                  # z ^= z >> 16
  li 6B CA EB 85 ml               # z *= $85EBCA6B
  du ls F3 sh xr                  # z ^= z >> 13
  li 35 AE B2 C2 ml               # z *= $C2B2AE35
  du ls F0 sh xr                  # z ^= z >> 16
  rt
:main rnd rnd hl
"#

/-- The control macros: `.i .e .t` and `.w .d .o`. -/
def demo : Asm.Program := b4a! r#"
:abs du c0 lt .i c0 sw sb .e .t rt                          # (n - |n|)
:tri c0 sw .w du c0 eq nt .d sw ov ad sw c1 sb .o zp rt     # (n - 1+2+..+n)
:m1 ls -5 abs lb 7 abs hl
:m2 lb 0A tri hl
"#

/-- Run a program from one of its labels, and show the stack. -/
def go (p : Asm.Program) (l : String) : List Int :=
  (dstack (run (setRST (setIP (p.load mkInitialState) (p.addr l)) 1))).map toInt32

-- The same numbers as the `rn` op: [-1832243442, 1020716019]
#eval go rnd "main"
-- [5, 7]
#eval go demo "m1"
-- [55]
#eval go demo "m2"
