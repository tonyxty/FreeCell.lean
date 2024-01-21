import SizedArray.Basic
import Deck.Basic

open Deck

@[reducible]
def Column := Array Card

structure Layout (c n : Nat) where
  columns : SizedArray Column c
  foundations : SizedArray (Option Rank) 4
  cells : SizedArray (Option Card) n

/- Define the "CanBuildOn" relation and show that it is decidable -/
section CanBuildOn
@[reducible]
def CanBuildOn' (c c' : Card) := c.rank = c'.rank + 1 ∧ c.color ≠ c'.color

inductive CanBuildOn : Card → Option Card → Prop :=
  | none : ∀ c, CanBuildOn c none
  | some : ∀ {c c'}, CanBuildOn' c c' → CanBuildOn c (some c')

def canBuildOn (c : Card) (c' : Option Card) : Bool :=
  match c' with
  | .none => true
  | .some c' => decide (CanBuildOn' c c')

-- FIXME: perhaps this decidablity can be automatically derived
variable {c : Card} {c' : Option Card}

theorem of_canBuildOn_eq_true {c : Card} {c' : Option Card} : canBuildOn c c' = true → CanBuildOn c c' :=
  match c' with
  | none => fun _ => .none c
  | some _ => fun h => .some (of_decide_eq_true h)

theorem of_canBuildOn_eq_false {c : Card} {c' : Option Card} : canBuildOn c c' = false → ¬CanBuildOn c c' := fun h h' =>
  match h' with
  | .none _ => by contradiction
  | .some h' => of_decide_eq_false h h'

instance : Decidable (CanBuildOn c c') :=
  match h : canBuildOn c c' with
  | true => isTrue (of_canBuildOn_eq_true h)
  | false => isFalse (of_canBuildOn_eq_false h)

end CanBuildOn
