{- |
  Slight variation on algorithm K.

  Instead of normal occurs-check, we perform a slightly optimised one.

  Optimisation comes from the fact that for each u-variable we store all
  variables it refers to. So to peform an occurs check we don't need to
  do full substitution on term, we only need to combine parent-sets of
  all vars in the term being checked.
-}
module Control.Unification.Unify
  ( -- * Unifier state
    UnifierState, emptyUnifierState

    -- * Monad
  , M, runM

    -- * Unifier
  , (=:=)

    -- * Type extractor
  , apply
  ) where

import Control.Monad (when, unless)
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Foldable (fold, for_)
import Data.Bifunctor (bimap)

import Control.Unification.Term
import Control.Unification.Error
import Control.Unification.Unifiable

{- |
  Keep bidings of uvars (unification variables), and keep track of
  parent-sets for uvars to be sure the state never has a cyclic type.
-}
data UnifierState t v = MkUnifierState
  { bindings     :: Map.Map v (Term t v)   -- Uvar bindings.
  , dependencies :: Map.Map v (Set.Set v)  -- Sets of "parent" uvars.
  }

{- |
  Clean state for the unifier.
-}
emptyUnifierState :: UnifierState t v
emptyUnifierState = MkUnifierState
  { bindings     = Map.empty
  , dependencies = Map.empty
  }

{- |
  Unification monad. ExceptT is above the State, so we the rollback will not
  destroy latest bindings, which we might use to improve error messages.
-}
type M t v
  = ExceptT (Error        t v)
  ( State   (UnifierState t v))

{- |
  Run unifier. Notice that despite the signature, errors don't cause rollback.
-}
runM
  :: M t v a
  -> UnifierState t v
  -> Either (Error t v) (a, UnifierState t v)
runM = (exchange .) . runState . runExceptT
  where
    exchange :: (Either e a, s) -> Either e (a, s)
    exchange (ma, s) = bimap id (, s) ma

{-
  Get variable binding, if any.
-}
find :: (Ord v) => v -> M t v (Maybe (Term t v))
find v = gets (Map.lookup v . (.bindings))

{-
  Bind the variable to the term.
-}
(=:) :: (Ord v) => v -> Term t v -> M t v ()
var =: term = do
  modify \s -> s
    { bindings = Map.insert var term s.bindings
    }

{-
  Add parents for the variable.
-}
(=?) :: (Ord v) => v -> Set.Set v -> M t v ()
var =? parents = do
  modify \s -> s
    { dependencies = Map.insertWith (<>) var parents s.dependencies
    }

{-
  Add binding, do shared occurs-check.

  We store parent uvars for each uvar present, so we can somewhat-quickly
  calculate the transtivie closure of reachability.

  We do that by taking all vars from the term and merging of their cached
  reachabilities.

  If the var being assigned is present in the result check, the term is cyclic
  and therefore is refuted.
-}
(=::) :: (Ord v, Traversable t) => v -> Term t v -> M t v ()
var =:: term = do
  deps <- gets (.dependencies)
  let near    = allVars term
  let allDeps = near <> foldMap (fold . (`Map.lookup` deps)) near

  when (var `Set.member` allDeps) do
    throwError Occurs {var, term}

  var =: term
  var =? allDeps

{- |
  Apply full sustitution to the term.
-}
apply :: (Ord v, Traversable t) => Term t v -> M t v (Term t v)
apply = \case
  Var var -> do
    find var >>= \case
      Nothing   -> return (Var var)
      Just term -> apply term

  Struct struct -> do
    Struct <$> traverse apply struct

{-
  If the term is a bound variable, turn either into structural term,
  or into a free variable. Re-bind the bound variable onto result, if it points
  to another variable.
-}
contractVarChain :: (Ord v, Traversable t) => Term t v -> M t v (Term t v)
contractVarChain = \case
  Struct struct -> return (Struct struct)
  Var var -> do
    find var >>= \case
      Nothing -> return (Var var)
      Just term -> do
        final <- contractVarChain term
        var =: final
        return final

{-
  Core of unification.
-}
unify :: (Ord v, Unifiable t) => Term t v -> Term t v -> M t v ()
unify expected got = do
  expected <- contractVarChain expected  -- contract both args
  got      <- contractVarChain got

  case (expected, got) of
    (Var left, Var right) -> do
      unless (left == right) do          -- different variables are merged
        left =: Var right

    (Var var, term)    -> var =: term    -- occurs-check + assign
    (term,    Var var) -> var =: term    -- -"-

    (Struct left, Struct right) -> do
      case match left right of           -- match outer redex
        Nothing -> do
          throwError Mismatch {expected, got}

        Just matched -> do
          for_ matched \(left', right') ->
            unify left' right'           -- perform unification on each subexpr

{- |
  Wrapper for unificator.

  Catch and re-throw errors, while applying latest knowledge to the types
  mentioned in the errors.
-}
(=:=) :: (Ord v, Unifiable t) => Term t v -> Term t v -> M t v ()
expected =:= got = do
  unify expected got `catchError` \case
    Occurs {var, term} -> do
      term <- apply term
      throwError Occurs {var, term}

    Mismatch {expected, got} -> do
      expected <- apply expected
      got      <- apply got
      throwError Mismatch {expected, got}