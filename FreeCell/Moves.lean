import FreeCell.Basic
import FreeCell.CanBuildOn

/-!
Defines and implements valid single-step moves
-/

open Deck

namespace Layout
variable {k n : Nat}
variable (τ : Layout k n)

/-- A card can reside at a column, a cell, or the foundation -/
inductive Pos (k n : Nat) where
  | column : Fin k → Pos k n
  | cell : Fin n → Pos k n
  | foundation : Pos k n

/-- but can be only moved from a nonempty column or cell -/
inductive From where
  | column (i : Fin k) : τ.columns[i].size > 0 → From
  | cell (i : Fin n) : τ.cells[i] ≠ .none → From

def validate : Pos k n → Option (From τ)
  | .column i => if h : τ.columns[i].size > 0 then .some <| .column i h else .none
  | .cell i => if h : τ.cells[i] ≠ .none then .some <| .cell i h else .none
  | .foundation => .none

def take (f : From τ) : Card × Layout k n :=
  match f with
  | .column i h =>
    let column := τ.columns[i]
    -- This should be called Array.back
    let card := column.get ⟨column.size.pred, Nat.pred_lt (Nat.ne_of_lt h).symm⟩
    ⟨ card, { τ with columns := τ.columns.set i column.pop } ⟩
  | .cell i h =>
    let card := match h' : τ.cells[i] with
      | .some c => c
      | .none => by contradiction
    ⟨ card, { τ with cells := τ.cells.set i .none } ⟩

def tryPut (c : Card) (t : Pos k n) : Option (Layout k n) :=
  match t with
  | .column j =>
    let column := τ.columns[j]
    if CanBuildOn c column.back? then
      .some { τ with columns := τ.columns.set j (column.push c) }
    else
      .none
  | .cell j =>
    if τ.cells[j] = .none then
      .some { τ with cells := τ.cells.set j (.some c) }
    else
      .none
  | .foundation =>
    let j := c.suit
    match c.rank.1 with
    | .zero =>
      if τ.foundations[j] = none then
        .some { τ with foundations := τ.foundations.set j (.some c.rank) }
      else
        .none
    | .succ r =>
      if τ.foundations[j].map Fin.val = .some r then
        .some { τ with foundations := τ.foundations.set j (.some c.rank) }
      else
        .none

def move (f t : Pos k n) : Option (Layout k n) := do
  let (card, τ') := τ.take <| ← τ.validate f
  τ'.tryPut card t

end Layout
