import FreeCell.Moves

/-!
Parse a string into a Pos or a From
-/

variable {k n : Nat}

def Layout.Pos.parse (s : String) : Option (Layout.Pos k n) := do
  let c ← s.get? 0
  if c == 'f' then
    return .foundation
  else if c == 'c' then
    let i ← s.toSubstring.drop 1 |>.toNat?
    if h : i < n then return .cell ⟨ i, h ⟩ else .none
  else
    let i ← s.toNat?
    if h : i < k then return .column ⟨ i, h ⟩ else .none
