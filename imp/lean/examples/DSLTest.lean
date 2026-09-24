import B4.Basic
import B4.DSL

open B4

def hiProg := b4! {
  lb 72 lb 101 -- 'H' 'e'
}

def main : IO Unit := do
  let simpleProg := b4! {
    lb 72  -- 'H'
    lb 101 -- 'e'
    111    -- 'o' (using num literal for li)
    ad     -- 'e' + 'o'
    @X     -- read register X
    !Y     -- write to register Y
    ^T     -- invoke register T
  }
  
  IO.println s!"Assembled {simpleProg.length} ops"
  for op in simpleProg do
    IO.println s!"Op byte: {op.toByte}"

  let bytes := assemble simpleProg
  IO.println s!"Total bytes: {bytes.size}"

#eval main
