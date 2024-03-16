import Deck.Basic

/-!
Defines the "CanBuildOn" relation, i.e., when a card can build upon another card, and show that it is decidable.
-/

namespace FreeCell

open Deck

@[reducible]
def CanBuildOn' (c c' : Card) : Prop := c.rank = c'.rank + 1 ∧ c.color ≠ c'.color

inductive CanBuildOn : Card → Option Card → Prop :=
  | none : ∀ c, CanBuildOn c none
  | some : ∀ {c c'}, CanBuildOn' c c' → CanBuildOn c (.some c')

def canBuildOn (c : Card) (c' : Option Card) : Bool :=
  match c' with
  | .none => true
  | .some c' => decide (CanBuildOn' c c')

-- FIXME: perhaps this decidablity can be automatically derived
variable {c : Card} {c' : Option Card}

theorem of_canBuildOn_eq_true : canBuildOn c c' = true → CanBuildOn c c' :=
  match c' with
  | .none => fun _ => .none c
  | .some _ => fun h => .some (of_decide_eq_true h)

theorem of_canBuildOn_eq_false : canBuildOn c c' = false → ¬CanBuildOn c c' := fun h h' =>
  match h' with
  | .none _ => by contradiction
  | .some h' => of_decide_eq_false h h'

instance : Decidable (CanBuildOn c c') :=
  match h : canBuildOn c c' with
  | true => isTrue (of_canBuildOn_eq_true h)
  | false => isFalse (of_canBuildOn_eq_false h)

end FreeCell
