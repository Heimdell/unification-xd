
{- |
  Outermost-layer unification.

  Functions like an "induction step" for the unifier.

  Since it has `Generic1` fallback, you can use @anyclass@ strategy
  to derive this interface.
-}
module Control.Unification.Unifiable
  ( Unifiable(..)
  )
  where

import Data.Traversable (for)
import GHC.Generics

{- |
  Ability to unify outermost layer.
-}
class (Traversable t) => Unifiable t where
  {- |
    If the outermost layers of two types collate,
    return a type with that layer, filled with pairs for delayed unification.
  -}
  match :: t a -> t a -> Maybe (t (a, a))

  default
    match
      :: ( Generic1 t
         , Unifiable (Rep1 t)
         )
      => t a
      -> t a
      -> Maybe (t (a, a))
  match ma mb = to1 <$> match (from1 ma) (from1 mb)

instance Unifiable V1 where
  match = \case

instance Unifiable U1 where
  match U1 U1 = Just U1

instance Unifiable Par1 where
  match (Par1 a) (Par1 b) = return $ Par1 (a, b)

instance (Unifiable f) => Unifiable (Rec1 f) where
  match (Rec1 a) (Rec1 b) = Rec1 <$> match a b

instance (Eq c) => Unifiable (K1 i c) where
  match (K1 a) (K1 b)
    | a == b    = return (K1 a)
    | otherwise = Nothing

instance (Unifiable f) => Unifiable (M1 i c f) where
  match (M1 a) (M1 b) = M1 <$> match a b

instance (Unifiable f, Unifiable g) => Unifiable (f :+: g) where
  match a b = case (a, b) of
    (L1 q, L1 w) -> L1 <$> match q w
    (R1 q, R1 w) -> R1 <$> match q w
    _            -> Nothing

instance (Unifiable f, Unifiable g) => Unifiable (f :*: g) where
  match (a :*: c) (b :*: d) = pure (:*:) <*> match a b <*> match c d

instance (Unifiable f, Unifiable g) => Unifiable (f :.: g) where
  match (Comp1 a) (Comp1 b) = do
    Comp1 <$> (traverse (uncurry match) =<< match a b)
