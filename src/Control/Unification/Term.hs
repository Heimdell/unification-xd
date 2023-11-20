
{- |
  Type skeleton.

  `Term` is used to construct a type out of some carrier-functor.
  It adds unification variables for free and substitution
  (with the hep of 'Unify' module).
-}
module Control.Unification.Term
  ( -- * Type
    Term (..)

    -- * Methods
  , allVars
  ) where

import Control.Monad (ap)
import Data.Set qualified as Set

{- |
  Free monad.
-}
data Term t v
  = Var     v              -- ^ Substitution point (flex)
  | Struct (t (Term t v))  -- ^ Sttructural node   (rigid)
  deriving stock (Functor, Foldable, Traversable)

instance (Show v, Show (t (Term t v))) => Show (Term t v) where
  showsPrec prec = \case
    Var    v -> showsPrec prec v
    Struct t -> showsPrec prec t

{- |
  Get all variables from the term.
-}
allVars :: (Ord v, Foldable t) => Term t v -> Set.Set v
allVars = foldMap Set.singleton