
{- | Default implementation for most interfaces.
-}

module Control.Monad.Inference.Carrier
  ( module Control.Monad.Inference.Carrier
  , module Data.Product
  ) where

import Control.Monad.RWS
import Control.Monad.Catch
import Data.Map qualified as Map
import Data.Map (Map)

import Control.Monad.Inference
import Data.Product

newtype InferT r w s m a = InferT
  { unInferT :: RWST r w s m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadWriter w)

instance
  ( Monad m
  , Variable i v
  , HasElem i s
  , Monoid w
  ) => MonadRefresh (InferT r w s m) v
  where
    fresh = do
      s <- InferT do gets (getElem @i @s)
      let (n, s') = generate s
      InferT do modify do modElem (const s')
      return n

instance
  ( Variable i v
  , Unifiable t
  , Monoid w
  , Monad m
  , HasElem i s
  , HasElem (UState t v) s
  )
  => BindingMonad (InferT r w s m) i t v
  where
    find v = InferT do Map.lookup v <$> gets (usMap . getElem)

    v =: t = do
      InferT do modify do modElem do \s -> s { usMap = Map.insert v t (usMap s) }
      return t

instance (Monoid w) => MonadTrans (InferT r w s) where
  lift = InferT . lift

runInferT :: r -> s -> InferT r w s m a -> m (a, s, w)
runInferT r s act = runRWST (unInferT act) r s

instance (HasElem (Context t v) r, Monad m, Monoid w) => HasContext t v (InferT r w s m) where
  askContext = InferT do asks getElem

  localContext f act = InferT do local (modElem f) $ unInferT act

{- | Inner state for default `BindingMonad` implementation.
-}
data UState t v = UState
  { usMap :: Map v (Term t v)
  }

deriving stock instance (Show a, Show (f Unshow), Functor f) => Show (UState f a)

{- | Default inner state for default `BindingMonad` implementation.
-}
emptyUState :: forall t v. Ord v => UState t v
emptyUState = UState mempty
