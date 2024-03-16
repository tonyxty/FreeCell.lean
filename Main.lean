import Deck
import FreeCell
import Lean.Data.Json

open Lean Deck FreeCell

def main : IO Unit := do
  let rng ← IO.stdGenRef.get
  let (a, r') := shuffle rng deck.toArray
  IO.stdGenRef.set r'
  let mut τ : Layout 8 4 := .fromArray a
  repeat do
    IO.println <| toJson τ |>.compress
    let f := (← (← IO.getStdin).getLine).trim
    let t := (← (← IO.getStdin).getLine).trim
    let τ' := Id.run <| OptionT.run do
      τ.move (← Layout.Pos.parse f) (← Layout.Pos.parse t)
    if let .some τ' := τ' then
      τ := τ'
    else
      IO.println "Invalid operation"
