import SizedArray.Basic
import Deck.Basic

open Deck

/- Define the "CanBuildOn" relation and show that it is decidable -/
section CanBuildOn
@[reducible]
def CanBuildOn' (c c' : Card) := c.rank = c'.rank + 1 ∧ c.color ≠ c'.color

inductive CanBuildOn : Card → Option Card → Prop :=
  | none : ∀ c, CanBuildOn c none
  | some : ∀ {c c'}, CanBuildOn' c c' → CanBuildOn c (.some c')

def canBuildOn (c : Card) (c' : Option Card) : Bool :=
  match c' with
  | .none => true
  | .some c' => decide (CanBuildOn' c c')

-- FIXME: perhaps this decidablity can be automatically derived
variable {c : Card} {c' : Option Card}

theorem of_canBuildOn_eq_true {c : Card} {c' : Option Card} : canBuildOn c c' = true → CanBuildOn c c' :=
  match c' with
  | .none => fun _ => .none c
  | .some _ => fun h => .some (of_decide_eq_true h)

theorem of_canBuildOn_eq_false {c : Card} {c' : Option Card} : canBuildOn c c' = false → ¬CanBuildOn c c' := fun h h' =>
  match h' with
  | .none _ => by contradiction
  | .some h' => of_decide_eq_false h h'

instance : Decidable (CanBuildOn c c') :=
  match h : canBuildOn c c' with
  | true => isTrue (of_canBuildOn_eq_true h)
  | false => isFalse (of_canBuildOn_eq_false h)

end CanBuildOn

@[reducible]
def Column := Array Card

structure Layout (c n : Nat) where
  columns : SizedArray Column c
  foundations : SizedArray (Option Rank) 4
  cells : SizedArray (Option Card) n

variable {k n : Nat}
namespace Layout
variable (τ : Layout k n)

/-- A card can be moved from a column or a cell --/
inductive From where
  | column (i : Fin k) : τ.columns[i].size > 0 → From
  | cell (i : Fin n) : τ.cells[i] ≠ none → From

/-- to a column, a cell, or a foundation --/
inductive To (k n : Nat) where
  | column : Fin k → To k n
  | foundation : To k n
  | cell : Fin n → To k n

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
  | .foundation =>
    let j := sorry
    let h : j < 4 := sorry
    match c.rank.1 with
    | .zero =>
      if τ.foundations[j] = none then
        .some { τ with foundations := τ.foundations.set ⟨j, h⟩ (.some c.rank) }
      else
        .none
    | .succ r =>
      if τ.foundations[j].map Fin.val = .some r then
        .some { τ with foundations := τ.foundations.set ⟨j, h⟩ (.some c.rank) }
      else
        .none
  | .cell j =>
    if τ.cells[j] = .none then
      .some { τ with cells := τ.cells.set j (.some c) }
    else
      .none

end Layout
