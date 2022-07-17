
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

module Fixpoint where

import Control.Monad ((<=<))
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Foldable (fold)

{- | Fixpoint of a functor f, with some context c at each node.
-}
newtype Fix f = Fix
  { unFix :: f (Fix f)
  }
  deriving stock (Generic)

deriving stock instance (Show (f (Fix f)))             => Show (Fix f)
deriving stock instance (Eq   (f (Fix f)))             => Eq   (Fix f)
deriving stock instance (Ord  (f (Fix f)))             => Ord  (Fix f)
deriving stock instance (Data (f (Fix f)), Typeable f) => Data (Fix f)

{- | Eliminator for `Fix`.
-}
cataFix :: (Functor f) => (f a -> a) -> Fix f -> a
cataFix alg = alg . fmap (cataFix alg) . unFix

{- | Eliminator for `Fix`, monadic.
-}
cataFixM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataFixM alg = do
  alg <=< traverse (cataFixM alg) . unFix

{- | A variant of `Fix` with some nodes replaced by @a@.

     It is a free monad, yes.
-}
data Term f a
  = Term (f (Term f a))
  | Var a
  deriving stock (Generic, Functor, Foldable, Traversable)

{- | Wrapper for showing the fixpoints.
-}
newtype Unshow = Unshow { unShow :: String }

instance Show Unshow where
  show = unShow

instance (Show a, Show (f Unshow), Functor f) => Show (Term f a) where
  show = unShow . cataTerm (Unshow . ("(" <>) . (<> ")") . show) (Unshow . show)

deriving stock instance (Eq   a, Eq   (f (Term f a)))             => Eq   (Term f a)
deriving stock instance (Ord  a, Ord  (f (Term f a)))             => Ord  (Term f a)
deriving stock instance (Data a, Data (f (Term f a)), Typeable f) => Data (Term f a)

{- | Eliminator for `Term`.
-}
cataTerm :: (Functor f) => (f b -> b) -> (a -> b) -> Term f a -> b
cataTerm node leaf = \case
  Term layer -> node (fmap (cataTerm node leaf) layer)
  Var a     -> leaf a

{- | Eliminator for `Term`, monadic.
-}
cataTermM :: (Traversable f, Monad m) => (f b -> m b) -> (a -> m b) -> Term f a -> m b
cataTermM node leaf = \case
  Term layer -> node =<< traverse (cataTermM node leaf) layer
  Var a -> leaf a

{- | Equip variable-less expression with variables.
-}
unfreeze :: (Functor f) => Fix f -> Term f a
unfreeze = cataFix Term

{- | Prove that there are no variables in term.

     This method ignores storage, apply binding beforehand.
-}
freeze :: (Traversable f) => Term f a -> Maybe (Fix f)
freeze = cataTermM (Just . Fix) (const Nothing)
