
{- | Default implementation for most interfaces.
-}

module Control.Monad.Inference.Carrier where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Control.Monad.Writer
import Data.Map qualified as Map
import Data.Map (Map)

import Control.Monad.Inference
import Data.Functor.Fixpoints

{- | Allows to refresh variables. Is a `StateT`, internally.
-}
newtype RefreshT store v m a = RefreshT
  { unRefreshT :: StateT store m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadThrow, MonadWriter e)

{- | Run variable-refreshing computation with an initial store.
-}
runRefreshT :: forall store v m a. (Monad m) => store -> RefreshT store v m a -> m a
runRefreshT s (RefreshT r) = evalStateT r s

{- | Is a designated inplementation for `MonadRefresh`.
-}
instance {-# OVERLAPS #-} (Variable store v, Monad m) => MonadRefresh (RefreshT store v m) v where
  fresh = do
    (n, s) <- RefreshT do gets generate
    RefreshT do put s
    return n

{- | Inner state for default `BindingMonad` implementation.
-}
data UState t v = UState
  { usMap :: Map v (Term t v)
  }

deriving stock instance (Show a, Show (f Unshow), Functor f) => Show (UState f a)

{- | Default inner state for default `BindingMonad` implementation.
-}
startUState :: Ord v => UState t v
startUState = UState mempty

{- | Default `BindingMonad` implementation.
-}
newtype UnificationT t v m a = UnificationT
  { runUnificationT :: StateT (UState t v) m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadReader r
    , MonadWriter w
    )

{- | @mtl@ moment
-}
instance MonadState s m => MonadState s (UnificationT t v m) where
  get = lift get
  put = lift . put

instance MonadTrans (UnificationT t v) where
  lift = UnificationT . lift

{- | Run inification, dump all guts.
-}
runUnification
  :: forall p t v a m
  .  (Ord v, MonadRefresh m v, MonadThrow m)
  => UnificationT t v m a
  -> m (a, UState t v)
runUnification unif
  = flip runStateT startUState
  $ runUnificationT unif

instance
  (Variable store v, Unifiable t, MonadRefresh m v)
  => BindingMonad (UnificationT t v m) store t v
  where
    find v = UnificationT do Map.lookup v <$> gets usMap

    v =: t = do
      UnificationT do modify \s -> s { usMap = Map.insert v t (usMap s) }
      return t

{- | The @mtl@ moment, again. On other hand, @mtl@ forces us to handle `UError`.
-}
type InferT t v m = UnificationT t v (ReaderT (Context t v) m)

{- | Run the machinery, dump all the gust on the floor.
-}
runInferT
  :: ( MonadThrow m
     , Ord v
     , MonadRefresh m v
     )
  => Context t v
  -> InferT t v m a
  -> m (a, UState t v)
runInferT ctx act = runReaderT (runUnification act) ctx
