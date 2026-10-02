import B4.Asm
import Lean

/-!
# b4a inline in Lean

`b4a! "..."` (or `b4a! org "..."`) is a block of b4a assembly written in a Lean
file. It is assembled when the file is compiled — an error in it is an error in
the Lean file — and stands for the `B4.Asm.Program` it assembles to:

```
def rnd : B4.Asm.Program := b4a! r#"
  :rnd li B9 79 37 9E lb B4 ri ad   # the seed, plus the golden ratio
    du lb B4 wi ...
"#
```
-/

namespace B4.Asm

open Lean Elab Term Meta

instance : ToExpr Program where
  toExpr p := mkApp3 (mkConst ``Program.mk) (toExpr p.writes) (toExpr p.labels) (toExpr p.org)
  toTypeExpr := mkConst ``Program

/-- b4a assembly, inline: assembled when the Lean file is compiled. -/
syntax (name := b4aTerm) "b4a!" (num)? str : term

@[term_elab b4aTerm] def elabB4a : TermElab := fun stx _ => do
  let org := match stx[1].getOptional? with
    | some n => n.isNatLit?.getD 0x100
    | none => 0x100
  let some src := stx[2].isStrLit? | throwError "b4a!: expected a string"
  match assemble src org with
  | .ok p => return toExpr p
  | .error e => throwError "b4a: {e}"

end B4.Asm
