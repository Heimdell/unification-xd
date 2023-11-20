
{- |
  Unification errors.
-}
module Control.Unification.Error where

import Control.Unification.Term

{- |
  Unification errors.
-}
data Error t v
  = Mismatch {expected, got  :: Term t v}  -- ^ Mismatch of outermost redexes
  | Occurs   {var :: v, term :: Term t v}  -- ^ Cyclic term

instance (Show v, Show (t (Term t v))) => Show (Error t v) where
  show = \case
    Mismatch {expected, got} ->
      unlines
        [ "expected term"
        , "  " <> show expected
        , "but got"
        , "  " <> show got
        ]

    Occurs {var, term} ->
      unlines
        [ "variable " <> show var <> " occurs recursively in term"
        , "  " <> show term
        ]