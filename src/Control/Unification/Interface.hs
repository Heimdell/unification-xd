{-# LANGUAGE FunctionalDependencies  #-}

module Control.Unification.Interface where

import Control.Unification.Unify
import Control.Unification.Unifiable

class (Unifiable t, Ord v, Monad m) => CanUnify m t v | m t -> v, m v -> t where
  liftUnify :: M t v a -> m a