universe u

def SizedArray (α : Type u) (n : Nat) := { a : Array α // a.size = n }

namespace SizedArray
variable {α : Type u}

def replicate (n : Nat) (v : α) : SizedArray α n := ⟨ Array.mkArray n v, Array.size_mkArray n v ⟩

variable {n : Nat}

private def castIndex (a : SizedArray α n) (i : Fin n) : Fin a.1.size :=
  (cast (congrArg Fin a.2.symm) i)

def set (a : SizedArray α n) (i : Fin n) (v : α) : SizedArray α n :=
  ⟨ a.1.set (a.castIndex i) v, (Array.size_set _ _ _).trans a.2 ⟩

instance : GetElem (SizedArray α n) Nat α fun _ i ↦ i < n where
  getElem a i h := a.1.get (a.castIndex ⟨i, h⟩)

end SizedArray
