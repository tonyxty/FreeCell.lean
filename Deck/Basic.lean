namespace Deck

abbrev Suit := Fin 4

notation "♠" => 0
notation "♥" => 1
notation "♦" => 2
notation "♣" => 3

instance : ToString Suit where
  toString s := .mk [#['♠', '♥', '♦', '♣'].get s]

inductive Color where
  | red : Color
  | black : Color
deriving DecidableEq

def Color.of (suit : Suit) : Color :=
  match suit with
  | ♠ => .black
  | ♥ => .red
  | ♦ => .red
  | ♣ => .black

abbrev Rank := Fin 13

structure Card where
  suit : Suit
  rank : Rank
deriving DecidableEq

instance : ToString Rank where
  toString r := .mk [#['A', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K'].get r]

instance : ToString Card where
  toString c := toString c.suit ++ toString c.rank

@[reducible]
def Card.color (card : Card) : Color := Color.of card.suit

end Deck
