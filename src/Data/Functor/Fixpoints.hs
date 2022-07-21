
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{- | Various fixpoints, mainly `Term`.

     This unification algorithm requires us to represent types as fixpoints with
     variables, so it can frely recure and do stuff.

     @Base@-functors, like in `recursion-schemes`, are outside of the scope of
      current package.
-}

module Data.Functor.Fixpoints where

import Control.Monad ((<=<))
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

{- | Fixpoint of a functor term, with some context c at each node.
-}
newtype Fix term = Fix
  { unFix :: term (Fix term)
  }
  deriving stock (Generic)

deriving stock instance (Show (term (Fix term)))             => Show (Fix term)
deriving stock instance (Eq   (term (Fix term)))             => Eq   (Fix term)
deriving stock instance (Ord  (term (Fix term)))             => Ord  (Fix term)
deriving stock instance (Data (term (Fix term)), Typeable term) => Data (Fix term)

{- | Eliminator for `Fix`.
-}
cataFix :: (Functor term) => (term res -> res) -> Fix term -> res
cataFix alg = alg . fmap (cataFix alg) . unFix

{- | Eliminator for `Fix`, monadic.
-}
cataFixM :: (Traversable term, Monad m) => (term res -> m res) -> Fix term -> m res
cataFixM alg = do
  alg <=< traverse (cataFixM alg) . unFix

{- | A variant of `Fix` with some nodes replaced by @var@.

     It is a free monad, yes.
-}
data Term term var
  = Term (term (Term term var))
  | Var var
  deriving stock (Generic, Functor, Foldable, Traversable)

{- | Wrapper for showing the fixpoints.
-}
newtype Unshow = Unshow { unShow :: String }

instance Show Unshow where
  show = unShow

instance (Show var, Show (term Unshow), Functor term) => Show (Term term var) where
  show = unShow . cataTerm (Unshow . show) (Unshow . show)

deriving stock instance (Eq   var, Eq   (term (Term term var)))             => Eq   (Term term var)
deriving stock instance (Ord  var, Ord  (term (Term term var)))             => Ord  (Term term var)
deriving stock instance (Data var, Data (term (Term term var)), Typeable term) => Data (Term term var)

{- | Eliminator for `Term`.
-}
cataTerm :: (Functor term) => (term res -> res) -> (var -> res) -> Term term var -> res
cataTerm node leaf = \case
  Term layer -> node (fmap (cataTerm node leaf) layer)
  Var a     -> leaf a

{- | Eliminator for `Term`, monadic.
-}
cataTermM :: (Traversable term, Monad m) => (term res -> m res) -> (var -> m res) -> Term term var -> m res
cataTermM node leaf = \case
  Term layer -> node =<< traverse (cataTermM node leaf) layer
  Var a -> leaf a

{- | Equip variable-less expression with variables.
-}
unfreeze :: (Functor term) => Fix term -> Term term var
unfreeze = cataFix Term

{- | Prove that there are no variables in term.

     This method ignores storage, apply binding beforehand.
-}
freeze :: (Traversable term) => Term term var -> Maybe (Fix term)
freeze = cataTermM (Just . Fix) (const Nothing)
