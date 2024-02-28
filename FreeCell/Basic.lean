import Lean.Data.Json
import SizedArray.Basic
import Deck.Basic

/-!
Defines FreeCell layouts
-/

open Deck
open Lean

abbrev Column := Array Card

structure Layout (c n : Nat) where
  columns : SizedArray Column c
  cells : SizedArray (Option Card) n
  foundations : SizedArray (Option Rank) 4

instance : ToJson Card where
  toJson card := toString card

instance (c n : Nat) : ToJson (Layout c n) where
  toJson layout := Json.mkObj [
    ("columns", toJson <| layout.columns.1),
    ("cells", toJson <| layout.cells.1),
    ("foundations", toJson <| layout.foundations.1.map λ o => (Option.casesOn o "" toString : String))
  ]
