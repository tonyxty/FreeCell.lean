import FreeCell.Basic
import FreeCell.CanBuildOn

/-!
Defines and implements valid single-step moves
-/

open Deck

namespace Layout
variable {k n : Nat}
variable (τ : Layout k n)

/-- A card can be moved from a nonempty column or a cell -/
inductive From where
  | column (i : Fin k) : τ.columns[i].size > 0 → From
  | cell (i : Fin n) : τ.cells[i] ≠ .none → From

/-- to a column, a cell, or a foundation -/
inductive To (k n : Nat) where
  | column : Fin k → To k n
  | cell : Fin n → To k n
  | foundation : To k n

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

def tryPut (c : Card) (t : To k n) : Option (Layout k n) :=
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

end Layout
