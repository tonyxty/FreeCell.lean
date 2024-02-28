namespace Deck

abbrev Suit := Fin 4

notation "♠" => 0
notation "♥" => 1
notation "♢" => 2
notation "♣" => 3

inductive Color where
  | red : Color
  | black : Color
deriving DecidableEq

def Color.of (suit : Suit) : Color :=
  match suit with
  | ♠ => .black
  | ♥ => .red
  | ♢ => .red
  | ♣ => .black

abbrev Rank := Fin 13

structure Card where
  suit : Suit
  rank : Rank
deriving DecidableEq

@[reducible]
def Card.color (card : Card) : Color := Color.of card.suit

end Deck
