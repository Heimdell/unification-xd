
module Ignore where

{- | We need to hide `Pos` from unifier, or else it will try to do `(==)` on it.
-}
newtype I a = I { unI :: a }
  deriving newtype (Show)

{- | We need to hide `Pos` from printig as well, because oh my.
-}
instance Eq   (I a) where _ == _ = True
instance Ord  (I a) where compare _ _ = EQ
