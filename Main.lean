import Deck
import FreeCell
import Lean.Data.Json

open Lean Deck

def main : IO Unit := do
  let rng ← IO.stdGenRef.get
  let (a, r') := shuffle rng deck.toArray
  IO.stdGenRef.set r'
  let τ : Layout 8 4 := .fromArray a
  IO.println <| toJson τ |>.compress
