import Lean.Data.Json
import Deck.Basic

/-!
Defines FreeCell layouts
-/

open Deck
open Lean

universe u
def Vector.replicate {α : Type u} {n : Nat} (x : α) : Vector α n :=
  ⟨ .ofFn (n := n) fun _ => x, Array.size_ofFn _ ⟩

namespace FreeCell

abbrev Column := Array Card

structure Layout (c n : Nat) where
  columns : Vector Column c
  cells : Vector (Option Card) n
  foundations : Vector (Option Rank) 4

def splitEvenly {α : Type} {n : Nat} (a : Array α) : Vector (Array α) n :=
  let k := a.size / n
  let m := a.size % n
  let rec go (i p : Nat) : Vector (Array α) i :=
    match i with
    | .zero => .mkEmpty n
    | .succ i =>
    let d := k + if i < m then 1 else 0
    go i (p + d) |>.push <| a.toSubarray p (p + d) |>.toArray
  go n 0

def Layout.fromArray {c n : Nat} (a : Array Card) : Layout c n :=
  ⟨ splitEvenly a, .replicate .none, .replicate .none ⟩

instance : ToJson Card where
  toJson card := toString card

instance : ToJson Rank where
  toJson r := toString r

instance (c n : Nat) : ToJson (Layout c n) where
  toJson layout := Json.mkObj [
    ("columns", toJson <| layout.columns.1),
    ("cells", toJson <| layout.cells.1),
    ("foundations", toJson <| layout.foundations.1)
  ]

end FreeCell
