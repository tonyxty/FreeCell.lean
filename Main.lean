import Deck
import FreeCell
import Lean.Data.Json

open Lean Deck

def main : IO Unit := do
  let rng ← IO.stdGenRef.get
  let (a, r') := shuffle rng deck.toArray
  IO.stdGenRef.set r'
  IO.println <| toJson a |>.compress
