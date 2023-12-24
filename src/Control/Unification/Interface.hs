{-# LANGUAGE FunctionalDependencies  #-}

module Control.Unification.Interface where

import Control.Unification.Unify

class Monad m => CanUnify m k v | m k -> v, m v -> k where
  liftUnify :: M k v a -> m a