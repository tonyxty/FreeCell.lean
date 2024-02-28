import SizedArray.Basic
import Deck.Basic

/-!
Defines FreeCell layouts
-/

open Deck

abbrev Column := Array Card

structure Layout (c n : Nat) where
  columns : SizedArray Column c
  foundations : SizedArray (Option Rank) 4
  cells : SizedArray (Option Card) n
