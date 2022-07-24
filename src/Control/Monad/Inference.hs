{-# language MonoLocalBinds #-}

{- | Various handles to piece together a Rank1 type inference.
-}

module Control.Monad.Inference
  ( -- * Unification
    Variable (..)
  , MonadRefresh (..)
  , Unifiable (..)
  , BindingMonad (..)
  , freshType
  , unify
  , generalise
  , instantiate
  , QTerm (..)
  , applyBindings
  , applyBindingsAll
  , refreshVars
  , refreshVarsAll

  -- * Contexts & inference
  , Context
  , HasContext (..)
  , CanUnify
  , CanUnify'
  , findVar
  , getContext
  , withFreshMonotypesFor
  , withMonotypes
  , withPolytypes
  , monotype

  -- * Glue for trees
  , module Data.Functor.Fixpoints

  -- * Errors
  , module Control.Monad.Inference.Errors
  ) where

import Control.Monad.State
import Control.Monad.Catch
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Foldable (fold)
import GHC.Generics

import Data.Functor.Fixpoints
import Control.Monad.Inference.Errors

{- | The working horse of unification.

     If the structure is the same, returns a collision, which is marked
     if it requires a recursive unification (`Right`) or not (`Left`).

     To have an instance, you need to

     > derive stock (Generic1, Functor, Foldable, Traversable)
     > derive anyclass (Unifiable)

     on your structure of choice.

-}
class Traversable term => Unifiable term where
  {- | Compare two instances of @term@, produce an unified one, if possible.

       Default implementation is copied
       from https://hackage.haskell.org/package/unification-fd,
       along with `Generic1` instances.
  -}
  match :: term any -> term any -> Maybe (term (Either any (any, any)))

  default
    match :: (Generic1 term, Unifiable (Rep1 term)) => term a -> term a -> Maybe (term (Either a (a, a)))
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

{- | Source of fresh type variables.
-}
class Monad m => MonadRefresh m var where
  -- | Produce a new type variable.
  fresh :: m var

{-
  I hate `lift`.
-}
instance {-# OVERLAPPABLE #-}
  ( MonadRefresh m var
  , MonadTrans h
  , Monad (h m)
  ) => MonadRefresh (h m) var
  where
    fresh = lift fresh

{- | Generate fresh type variable.
-}
freshType :: forall m term var. MonadRefresh m var => m (Term term var)
freshType = Var <$> fresh

{- | Variable, can be uniquely generated from some predefined store.
-}
class (Ord var) => Variable store var | var -> store where
  generate :: store -> (var, store)

{- | A monad that can store "substitutions".
-}
class
  ( Variable store var
  , Unifiable term
  , MonadRefresh m var
  )
  => BindingMonad m store term var
  | var -> store
  where
    {- | Find binding for type variable, if any.
    -}
    find  :: var -> m (Maybe (Term term var))

    {- | Update existing type variable.
    -}
    (=:)  :: var -> Term term var -> m (Term term var)

{- | Bind type to new variable.
-}
new :: BindingMonad m store term var => Term term var -> m var
new t = do
  v <- fresh
  _ <- v =: t
  return v

{- | I hate `lift`.
-}
instance {-# OVERLAPPABLE #-}
  ( BindingMonad m store term var
  , MonadTrans h
  , Monad (h m)
  ) => BindingMonad (h m) store term var
  where
    find  =  lift    .  find
    (=:)  = (lift .) . (=:)

{- | Break chains like @a -> b@, @b -> Int@, make all vars in chain refer to
     the last term instead (e.g. @a -> Int@, @b -> Int@).

     Internal function. If you think that's what you need, consider `semiprune`.
-}
-- prune :: BindingMonad m store term var => Term term var -> m (Term term var)
-- prune = \case
--   t@Term{} -> return t
--   Var v -> do
--     find v >>= \case
--       Just v' -> do
--         t <- prune v'
--         v =: t
--       Nothing -> return (Var v)

{- | Break chains like @a -> b@, @b -> c@, @c -> Int@, make all vars in chain
     refer to the last /variable/ instead (e.g. @a -> c@, @b -> c@, @c -> Int@).

     Internal function.
-}
semiprune :: BindingMonad m store term var => Term term var -> m (Term term var)
semiprune t = case t of
  Term{}   -> return t
  Var v -> loop v t
    where
      loop v0 t0 = do
        find v0 >>= \case
          Nothing     -> return t0
          Just Term{} -> return t0
          Just t'@(Var v') -> do
            final <- loop v' t'
            v0 =: final

{- | Analog to occurs-check that is waaay faster.
-}
newtype VisitedSetT term var m a = VisitedSetT
  { runVisitedSetT :: StateT (Map var (term (Term term var))) m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    )

instance MonadTrans (VisitedSetT term var) where
  lift = VisitedSetT . lift

{- | Register variable as seen bound to given type.
-}
class (Monad m) => MonadOccurs m pos term var where
  seenAs :: pos -> var -> term (Term term var) -> m ()

instance (MonadThrow m, Ord var, Exception (UError pos term var)) => MonadOccurs (VisitedSetT term var m) pos term var where
  seenAs p v t = do
    vars <- VisitedSetT get
    case Map.lookup v vars of
      Just t' -> throwM $ Occurs p v $ Term t'
      Nothing -> VisitedSetT $ modify $ Map.insert v t

{- | Run algorithm with visited-set enabled.
-}
visitedSet :: Monad m => VisitedSetT term var m a -> m a
visitedSet act = evalStateT (runVisitedSetT act) Map.empty

{- | Get all free variables.
-}
getFreeVars
  :: forall m store term var
  .  (BindingMonad m store term var)
  => Term term var -> m [var]
getFreeVars t = getFreeVars' ((), t)

{- | Get all free variables (shared).
-}
getFreeVars'
  :: forall m store term var list
  .  (BindingMonad m store term var, Traversable list)
  => list (Term term var) -> m [var]
getFreeVars' list = do
  idSet <- evalStateT (fold <$> traverse loop list) Set.empty
  return $ Set.toList idSet
  where
    loop :: Term term var -> StateT (Set var) m (Set var)
    loop t = do
      semiprune t >>= \case
        Term t' -> fold <$> traverse loop t'
        Var v -> do
          done <- gets (Set.member v)
          if done then return mempty
          else do
            modify (Set.insert v)
            find v >>= \case
              Nothing -> return $ Set.singleton v
              Just t'  -> loop t'

{- | Visitor pattern over storage.
-}
visitOnce
  :: ( MonadState (Map var (Either (Term term var) (Term term a))) m
     , MonadThrow m
     , BindingMonad m store term var
     , CanThrowUError pos term var
     )
  => pos
  -> (var -> m (Term term a))
  -> (var -> Term term var -> m (Term term a))
  -> Term term var
  -> m (Term term a)
visitOnce p leaf node = loop
  where
    loop t = do
      semiprune t >>= \case
        Term layer -> Term <$> traverse loop layer
        Var v -> do
          gets (Map.lookup v) >>= \case
            Just (Right t') -> return t'
            Just (Left  t') -> throwM $ Occurs p v t'
            Nothing -> do
              find v >>= \case
                Nothing -> leaf v
                Just t' -> do
                  modify $ Map.insert v $ Left t'
                  t'' <- node v t'
                  modify $ Map.insert v $ Right t''
                  return t''

{- | Rank1-polytype -}
data QTerm term var = Forall
  { qtArgs :: [var]          -- ^ Type arguments.
  , qtBody :: Term term var  -- ^ Body of the polytype.
  }

deriving stock instance (Show (term Unshow), Show var, Functor term) => Show (QTerm term var)
deriving stock instance (Eq  (term (Term term var)), Eq  var) => Eq  (QTerm term var)
deriving stock instance (Ord (term (Term term var)), Ord var) => Ord (QTerm term var)

{- | Assume that all non-yet-bound variables of the type are polymorphic.

     Generates a Rank1-polytype.
-}
generalise
  :: forall m store term var
  .  (CanUnify' term var store m)
  => Term term var -> m (QTerm term var)
generalise t = do
  vs <- getFreeVars t
  return $ Forall vs t

{- | Lift a monotype into polytype.
-}
monotype :: Term term var -> QTerm term var
monotype = Forall []

{- | Monomorphise a Rank1-polytype, producing a monotype with fresh vars.
-}
instantiate
  :: forall m store pos term var
  .  (CanUnify pos term var store m)
  => pos -> QTerm term var -> m (Term term var)
instantiate _ (Forall [] t) = return t
instantiate p (Forall _ t) = do
  refreshVars p t

{- | Apply all knowledge collected till that moment.
-}
applyBindings
  :: forall m store pos term var
  .  (CanUnify pos term var store m)
  => pos -> Term term var -> m (Term term var)
applyBindings p t = snd <$> applyBindingsAll p ((), t)

{- | Apply all knowledge collected till that (shared).
-}
applyBindingsAll
  :: forall m store pos term var list
  .  (CanUnify pos term var store m, Traversable list)
  => pos -> list (Term term var) -> m (list (Term term var))
applyBindingsAll p list = do
  evalStateT (traverse loop list) Map.empty
  where
    loop = visitOnce p (return . Var) \_ -> loop

{- | Refresh all variables.
-}
refreshVars
  :: forall m store pos term var
  .  (CanUnify pos term var store m)
  => pos -> Term term var -> m (Term term var)
refreshVars p t = do
  ((), t') <- refreshVarsAll p ((), t)
  return t'

{- | Refresh all variables (shared).
-}
refreshVarsAll
  :: forall m store pos term var list
  .  (CanUnify pos term var store m, Traversable list)
  => pos -> list (Term term var) -> m (list (Term term var))
refreshVarsAll p list = do
  evalStateT (traverse loop list) Map.empty
  where
    loop = visitOnce p
      do \v -> do
          v' <- freshType
          modify $ Map.insert v $ Right v'
          return v'
      do \v t -> do
          modify $ Map.insert v $ Left t
          t' <- loop t
          v' <- Var <$> new t'
          modify $ Map.insert v $ Right v'
          return v'

{- | Unify two types, produce one type that is supertype of both.

     Update the storage with information gained.
-}
unify
  :: forall m store pos term var
  .  (CanUnify pos term var store m)
  => pos -> Term term var -> Term term var -> m (Term term var)
unify p tl0' tr0' =
  visitedSet do
    loop tl0' tr0'

  where
    loop :: Term term var -> Term term var -> VisitedSetT term var m (Term term var)
    loop tl00 tr00 = do
      tl0 <- semiprune tl00
      tr0 <- semiprune tr00
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
                    seenAs p vl tl
                    seenAs p vr tr
                    matches tl tr
                  _ <- vr =: t
                  vl =: tr0

                _ -> error "unify: impossible"

        (Var vl, Term tr) -> do
          t <- find vl >>= \case
            Nothing -> return tr0
            Just (Term tl) -> do
              rollback do
                seenAs p vl tl
                matches tl tr
            _ -> error "unify: impossible"

          _ <- vl =: t
          return tl0

        (Term tl, Var vr) -> do
          t <- find vr >>= \case
            Nothing -> return tl0
            Just (Term tr) -> do
              rollback do
                seenAs p vr tr
                matches tl tr
            _ -> error "unify: impossible"

          _ <- vr =: t
          return tr0

        (Term tl, Term tr) -> do
          matches tl tr

    matches tl tr = do
      case match tl tr of
        Nothing  -> throwM $ Mismatch p (Term tl) (Term tr)
        Just tlr -> Term <$> traverse loop_ tlr

    loop_ (Left t) = return t
    loop_ (Right (tl, tr)) = loop tl tr

{- | Do action, then undo state changes.
-}
rollback :: Monad m => VisitedSetT term var m a -> VisitedSetT term var m a
rollback act = do
  s <- VisitedSetT get
  r <- act
  VisitedSetT do put s
  return r

{- | Various events that can stop the inference process.

     Each of them points to a source location.
-}
data Undefined pos n = Undefined pos n                -- ^ Variable was not declared.
  deriving stock (Show)
  deriving anyclass (Exception)

class (Ord v', Show v', Typeable v', Monad m) => HasContext v' t v m where
  askContext :: m (Context v' t v)
  localContext :: (Context v' t v -> Context v' t v) -> m a -> m a

{- | Umbrella-style context that allows machinery from this module to work.
-}
type CanUnify' term var store m =
  ( MonadThrow m
  , BindingMonad m store term var
  )

{- | Umbrella-style context that allows machinery from this module to work.
-}
type CanUnify pos term var store m =
  ( CanUnify' term var store m
  , CanThrowUError pos term var
  )
{- | @Var : Type@ relation.
-}
type Context var' term var = Map var' (QTerm term var)

{- | Lookup for a variable.

     If found, instantiate its type.

     Otherwise, throw `Undefined` with given position.
-}
findVar :: (CanUnify pos term var store m, HasContext var' term var m) => pos -> var' -> m (Term term var)
findVar p n = do
  Map.lookup n <$> askContext >>= \case
    Nothing -> throwM $ Undefined p n
    Just qt -> instantiate p qt

{- | Generate fresh monotypes for given names.
-}
withFreshMonotypesFor :: forall term var var' store m a. (CanUnify' term var store m, HasContext var' term var m) => [var'] -> m a -> m a
withFreshMonotypesFor names act = do
  delta <- for names \n -> do
    t <- freshType @m @term @var
    return (n, t)

  withMonotypes delta act

{- | Add some monotyped declaration into the context.
-}
withMonotypes :: (CanUnify' term var store m, HasContext var' term var m) => [(var', Term term var)] -> m a -> m a
withMonotypes delta = do
  withPolytypes $ (fmap.fmap) monotype delta

{- | Add some polytyped declaration into the context.
-}
withPolytypes :: (CanUnify' term var store m, HasContext var' term var m) => [(var', QTerm term var)] -> m a -> m a
withPolytypes delta act = do
  let dCtx = Map.fromList delta
  localContext (dCtx <>) do
    act

getContext :: forall v' t v m. (HasContext v' t v m) => m [v']
getContext = do
  ctx <- askContext @v' @t @v
  return (Map.keys ctx)
