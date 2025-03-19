def shuffle {g : Type} [RandomGen g] {α : Type} (rng : g) (a : Array α) : Array α × g := Id.run do
  let mut r := rng
  let mut a := a
  let n := a.size
  for i in [:n] do
    let res := randNat r 0 (n - 1 - i)
    r := res.2
    a := a.swapIfInBounds i (i + res.1)
  return (a, r)
