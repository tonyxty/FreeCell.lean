namespace Deck

inductive Suit where
  | spades : Suit
  | hearts : Suit
  | diamonds : Suit
  | clubs : Suit
deriving DecidableEq

notation "♠" => Suit.spades
notation "♥" => Suit.hearts
notation "♢" => Suit.diamonds
notation "♣" => Suit.clubs

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

@[reducible]
def Rank := Fin 13

structure Card where
  suit : Suit
  rank : Rank
deriving DecidableEq

@[reducible]
def Card.color (card : Card) : Color := Color.of card.suit

end Deck
