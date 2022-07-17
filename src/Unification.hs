
{- | Unification methods.

     Contains all the stuff you need (you also need to import 'Fixpoint').
-}

module Unification where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Functor.Compose
import Data.Foldable (fold)
import Data.Traversable (for)
import GHC.Generics
import Lens.Micro.Platform
import Shower

import Fixpoint

{- | The working horse of unification.

     If the structure is the same, returns a collision, which is marked
     if it requires a recursive unification (`Right`) or not (`Left`).
-}
class Traversable f => Unifiable f where
  match :: f a -> f a -> Maybe (f (Either a (a, a)))

  {- | Copied from @unification-fd@, along with `Generic1` instances.
  -}
  default
    match :: (Generic1 f, Unifiable (Rep1 f)) => f a -> f a -> Maybe (f (Either a (a, a)))
  match a b = to1 <$> match (from1 a) (from1 b)

instance Unifiable V1 where
  match a _ = return $ Left <$> a

instance Unifiable U1 where
  match a _ = return $ Left <$> a

instance Unifiable Par1 where
  match (Par1 a) (Par1 b) = return $ Par1 $ Right (a, b)

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
    res' <- match a b
    res'' <- for res' \case
      Left ga -> return $ Left <$> ga
      Right (ga, gb) -> match ga gb
    return $ Comp1 res''

{- | I have decided to split fresh name generation away from `BindingMonad`
     for my convenience.
-}
class Monad m => MonadRefresh m v where
  fresh :: m v

{-
  I hate `lift`.
-}
instance {-# OVERLAPPABLE #-}
  ( MonadRefresh m v
  , MonadTrans h
  , Monad (h m)
  ) => MonadRefresh (h m) v
  where
    fresh = lift fresh

{- | A monad that can store "substitutions". Variables no longer required to be
     `Int`. Some speed is lost, much convenience is gained.
-}
class
  ( Ord v
  , Unifiable t
  , MonadRefresh m v
  )
  => BindingMonad m t v
  | m t -> v
  , m v -> t
  where
    {- | Find binding for type variable, if any.
    -}
    find  :: v -> m (Maybe (Term t v))

    {- | Update existing type variable.
    -}
    (=:)  :: v -> Term t v -> m (Term t v)

{- | Bind type to new variable.
-}
new :: BindingMonad m t v => Term t v -> m v
new t = do
  v <- fresh
  v =: t
  return v

{- | I hate `lift`.
-}
instance {-# OVERLAPPABLE #-}
  ( BindingMonad m t v
  , MonadTrans h
  , Monad (h m)
  ) => BindingMonad (h m) t v
  where
    find  =  lift    .  find
    (=:)  = (lift .) . (=:)

{- | Inner state for default `BindingMonad` implementation.
-}
data UState t v = UState
  { _usMap :: Map v (Term t v)
  }

deriving stock instance (Show a, Show (f Unshow), Functor f) => Show (UState f a)

makeLenses ''UState

{- | Default inner state for default `BindingMonad` implementation.
-}
startUState :: Ord v => UState t v
startUState = UState mempty

{- | Unification error.
-}
data UError t v
  = {- | Variable is bound to a type that refers to the variable itself.

         We don't support recursive types.
    -}
    Occurs v (Term t v)

  | {- | We expected one type, but got something else.
    -}
    Mismatch (Term t v) (Term t v)

deriving stock instance (Show a, Show (f Unshow), Functor f) => Show (UError f a)

{- | Default `BindingMonad` implementation.
-}
newtype UnificationT t v m a = UnificationT
  { runUnificationT
      :: StateT (UState t v)
       ( ExceptT (UError t v)
         m ) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError (UError t v)
    , MonadReader r
    , MonadWriter w
    )

{- | @mtl@ moment
-}
instance MonadState s m => MonadState s (UnificationT t v m) where
  get = lift get
  put = lift . put

instance MonadTrans (UnificationT t v) where
  lift = UnificationT . lift . lift

{- | Run inification, dump all guts.
-}
runUnification :: forall t v a m. (Ord v, MonadRefresh m v) => UnificationT t v m a -> m (Either (UError t v) (a, UState t v))
runUnification unif
  = runExceptT
  $ flip runStateT startUState
  $ runUnificationT unif

instance
  (Ord v, Unifiable t, MonadRefresh m v)
  => BindingMonad (UnificationT t v m) t v
  where
    find v = UnificationT do Map.lookup v <$> use usMap

    v =: t = do
      UnificationT do usMap %= Map.insert v t
      return t

{- | Break chains like @a -> b@, @b -> Int@, make all vars in chain refer to
     the last term instead (e.g. @a -> Int@, @b -> Int@).

     Internal function. If you think that's what you need, consider `semiprune`.
-}
prune :: BindingMonad m t v => Term t v -> m (Term t v)
prune = \case
  t@Term{} -> return t
  Var v -> do
    find v >>= \case
      Just v' -> do
        t <- prune v'
        v =: t
      Nothing -> return (Var v)

{- | Break chains like @a -> b@, @b -> c@, @c -> Int@, make all vars in chain
     refer to the last /variable/ instead (e.g. @a -> c@, @b -> c@, @c -> Int@).

     Internal function.
-}
semiprune :: BindingMonad m t v => Term t v -> m (Term t v)
semiprune t = case t of
  Term{}   -> return t
  Var v -> loop v t
    where
      loop v0 t0 = do
        find v0 >>= \case
          Nothing     -> return t0
          Just Term{} -> return t0
          Just t@(Var v) -> do
            final <- loop v t
            v0 =: final

{- | Analog to occurs-check that is waaay faster.
-}
newtype VisitedSetT t v m a = VisitedSetT
  { runVisitedSetT :: StateT (Map v (t (Term t v))) m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError e
    )

instance MonadTrans (VisitedSetT t v) where
  lift = VisitedSetT . lift

{- | Register variable as seen bound to given type.
-}
class (Monad m) => MonadOccurs m t v where
  seenAs :: v -> t (Term t v) -> m ()

instance (MonadError (UError t v) m, Ord v) => MonadOccurs (VisitedSetT t v m) t v where
  seenAs v t = do
    vars <- VisitedSetT get
    case Map.lookup v vars of
      Just t -> throwError $ Occurs v $ Term t
      Nothing -> VisitedSetT $ modify $ Map.insert v t

{- | Run algorithm with visited-set enabled.
-}
visitedSet :: Monad m => VisitedSetT t v m a -> m a
visitedSet act = evalStateT (runVisitedSetT act) Map.empty

{- | Get all free variables.
-}
getFreeVars
  :: forall m t v
  .  (BindingMonad m t v)
  => Term t v -> m [v]
getFreeVars t = getFreeVars' ((), t)

{- | Get all free variables (shared).
-}
getFreeVars'
  :: forall m t v list
  .  (BindingMonad m t v, Traversable list)
  => list (Term t v) -> m [v]
getFreeVars' list = do
  idSet <- evalStateT (fold <$> traverse loop list) Set.empty
  return $ Set.toList idSet
  where
    varsOf :: Foldable f => f v -> [v]
    varsOf = Set.toList . foldMap Set.singleton

    loop :: Term t v -> StateT (Set v) m (Set v)
    loop t = do
      semiprune t >>= \case
        Term t -> fold <$> traverse loop t
        Var v -> do
          done <- gets (Set.member v)
          if done then return mempty
          else do
            modify (Set.insert v)
            find v >>= \case
              Nothing -> return $ Set.singleton v
              Just t  -> loop t

{- | Visitor pattern over storage.
-}
visitOnce
  :: ( MonadState (Map v (Either (Term t v) (Term t a))) m
     , MonadError (UError t v) m
     , BindingMonad m t v
     )
  => (v -> m (Term t a))
  -> (Term t v -> m (Term t a))
  -> Term t v
  -> m (Term t a)
visitOnce leaf node = loop
  where
    loop t = do
      semiprune t >>= \case
        Term layer -> Term <$> traverse loop layer
        Var v -> do
          gets (Map.lookup v) >>= \case
            Just (Right t) -> return t
            Just (Left  t) -> throwError $ Occurs v t
            Nothing -> do
              find v >>= \case
                Nothing -> leaf v
                Just t -> do
                  modify $ Map.insert v $ Left t
                  t' <- node t
                  modify $ Map.insert v $ Right t'
                  return t'

{- | Refresh only the given variables in the term.

     `refreshVars` is either broken or I can't figure out how to use it.
-}
instantiate
  :: forall m t v
  .  (BindingMonad m t v, MonadError (UError t v) m)
  => [v] -> Term t v -> m (Term t v)
instantiate vs t = do
  delta <- for vs \v -> do
    v' <- fresh
    return (v, v')

  let d = Map.fromList delta

  evalStateT (loop d t) Map.empty
  where
    loop d t = visitOnce (replace d) (loop d) t

    replace d v =
      case Map.lookup v d of
        Just r -> return (Var r)
        Nothing -> return (Var v)

{- | Apply all knowledge collected till that moment.
-}
applyBindings
  :: forall m t v
  .  (BindingMonad m t v, MonadError (UError t v) m)
  => Term t v -> m (Term t v)
applyBindings t = snd <$> applyBindings' ((), t)

{- | Apply all knowledge collected till that (shared).
-}
applyBindings'
  :: forall m t v list
  .  (BindingMonad m t v, Traversable list, MonadError (UError t v) m)
  => list (Term t v) -> m (list (Term t v))
applyBindings' list = do
  evalStateT (traverse loop list) Map.empty
  where
    loop = visitOnce (return . Var) loop

{- | Refresh all variables.
-}
refreshVars
  :: forall m t v
  .  (BindingMonad m t v, MonadError (UError t v) m)
  => Term t v -> m (Term t v)
refreshVars t = do
  ((), t) <- refreshVars' ((), t)
  return t

{- | Refresh all variables (shared).
-}
refreshVars'
  :: forall m t v list
  .  (BindingMonad m t v, Traversable list, MonadError (UError t v) m)
  => list (Term t v) -> m (list (Term t v))
refreshVars' list = do
  evalStateT (traverse loop list) Map.empty
  where
    loop = visitOnce
      do \v -> do
          v' <- Var <$> fresh
          modify $ Map.insert v $ Right v'
          return v'
      do \t -> do
          t' <- loop t
          v' <- Var <$> new t'
          return v'

{- | Unify two types, produce one type that is supertype of both.

     Update the storage with information gained.
-}
unify
  :: forall m t v
  .  (BindingMonad m t v, MonadError (UError t v) m)
  => Term t v -> Term t v -> m (Term t v)
unify tl0 tr0 =
  visitedSet do
    loop tl0 tr0

  where
    loop :: Term t v -> Term t v -> VisitedSetT t v m (Term t v)
    loop tl0 tr0 = do
      tl0 <- semiprune tl0
      tr0 <- semiprune tr0
      case (tl0, tr0) of
        (Var vl, Var vr)
          | vl == vr  -> return tr0
          | otherwise -> do
              mtl <- find vl
              mtr <- find vr
              case (mtl, mtr) of
                (Nothing, Nothing) -> vl =: tr0
                (Nothing, Just _)  -> vl =: tr0
                (Just _,  Nothing) -> vr =: tl0

                (Just (Term tl), Just (Term tr)) -> do
                  t <- rollback do
                    vl `seenAs` tl
                    vr `seenAs` tr
                    matches tl tr
                  vr =: t
                  vl =: tr0

        (Var vl, Term tr) -> do
          t <- find vl >>= \case
            Nothing -> return tr0
            Just (Term tl) -> do
              rollback do
                vl `seenAs` tl
                matches tl tr
          vl =: t
          return tl0

        (Term tl, Var vr) -> do
          t <- find vr >>= \case
            Nothing -> return tl0
            Just (Term tr) -> do
              rollback do
                vr `seenAs` tr
                matches tl tr
          vr =: t
          return tr0

        (Term tl, Term tr) -> do
          matches tl tr

    matches tl tr = do
      case match tl tr of
        Nothing  -> throwError $ Mismatch (Term tl) (Term tr)
        Just tlr -> Term <$> traverse loop_ tlr

    loop_ (Left t) = return t
    loop_ (Right (tl, tr)) = loop tl tr

{- | Do action, then undo state changes.
-}
rollback :: Monad m => VisitedSetT t v m a -> VisitedSetT t v m a
rollback act = do
  s <- VisitedSetT get
  r <- act
  VisitedSetT do put s
  return r
