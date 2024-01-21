import Lake
open Lake DSL

package «FreeCell» where
  leanOptions := #[
    ⟨ `autoImplicit, false ⟩,
    ⟨ `relaxedAutoImplicit, false ⟩
  ]

lean_lib «FreeCell» where

lean_lib «Deck»

lean_lib «SizedArray»

@[default_target]
lean_exe «freecell» where
  root := `Main
