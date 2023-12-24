{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{- |
  Slight variation on algorithm K.

  Instead of normal occurs-check, we perform a slightly optimised one.

  Optimisation comes from the fact that for each u-variable we store all
  variables it refers to. So to peform an occurs check we don't need to
  do full substitution on term, we only need to combine parent-sets of
  all vars in the term being checked.
-}
module Control.Unification.Unify
  -- ( -- * Unifier state
  --   UnifierState, emptyUnifierState

  --   -- * Monad
  -- , M, runM

  --   -- * Unifier
  -- , (=:=)

  --   -- * Type extractor
  -- , apply
  -- )
  where

import Control.Monad (when, unless)
import Polysemy
import Polysemy.State
import Polysemy.Error
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

data Unifies t v m a where
  Apply :: Term t v -> Unifies t v m (Term t v)
  (:=:=) :: Term t v -> Term t v -> Unifies t v m ()
  Save :: Unifies t v m (UnifierState t v)
  Restore :: UnifierState t v -> Unifies t v m ()

makeSem ''Unifies

type Backend t v r =
  ( Members
    '[ State (UnifierState     t v)
     , Error (UnificationError t v)
     ] r
  , Ord v
  , Unifiable t
  )

runUnification
  :: Backend t v r
  => Sem (Unifies t v : r) a
  -> Sem                r  a
runUnification = interpret \case
  Apply t   -> applyImpl t
  l :=:= r  -> unifyImpl l r
  Save      -> get
  Restore s -> put s

{-
  Get variable binding, if any.
-}
find :: Backend t v r => v -> Sem r (Maybe (Term t v))
find v = gets @(UnifierState _ _) (Map.lookup v . (.bindings))

{-
  Bind the variable to the term.
-}
(=:) :: Backend t v r => v -> Term t v -> Sem r ()
var =: term = do
  modify \s -> s
    { bindings = Map.insert var term s.bindings
    }

{-
  Add parents for the variable.
-}
(=?) :: forall t v r. Backend t v r => v -> Set.Set v -> Sem r ()
var =? parents = do
  modify @(UnifierState t v) \s -> s
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
(=::) :: forall t v r. Backend t v r => v -> Term t v -> Sem r ()
var =:: term = do
  deps <- gets @(UnifierState t _) (.dependencies)
  let near    = allVars term
  let allDeps = near <> foldMap (fold . (`Map.lookup` deps)) near

  when (var `Set.member` allDeps) do
    throw Occurs {var, term}

  var =: term
  (=?) @t var allDeps

{- |
  Apply full sustitution to the term.
-}
applyImpl :: Backend t v r => Term t v -> Sem r (Term t v)
applyImpl = \case
  Var var -> do
    find var >>= \case
      Nothing   -> return (Var var)
      Just term -> applyImpl term

  Struct struct -> do
    Struct <$> traverse applyImpl struct

{-
  If the term is a bound variable, turn either into structural term,
  or into a free variable. Re-bind the bound variable onto result, if it points
  to another variable.
-}
contractVarChain :: Backend t v r => Term t v -> Sem r (Term t v)
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
unify :: forall t v r. Backend t v r => Term t v -> Term t v -> Sem r ()
unify expected got = do
  expected <- contractVarChain expected  -- contract both args
  got      <- contractVarChain got

  case (expected, got) of
    (Var left, Var right) -> do
      unless (left == right) do          -- different variables are merged
        (=::) @t left (Var right)

    (Var var, term)    -> (=::) @t var term    -- occurs-check + assign
    (term,    Var var) -> (=::) @t var term    -- -"-

    (Struct left, Struct right) -> do
      case match left right of           -- match outer redex
        Nothing -> do
          throw Mismatch {expected, got}

        Just matched -> do
          for_ matched \(left', right') ->
            unify left' right'           -- perform unification on each subexpr

{- |
  Wrapper for unificator.

  Catch and re-throw errors, while applying latest knowledge to the types
  mentioned in the errors.
-}
unifyImpl :: forall t v r. Backend t v r => Term t v -> Term t v -> Sem r ()
unifyImpl expected got = do
  unify expected got `catch` \case
    Occurs {var, term} -> do
      term <- applyImpl @t @v term
      throw Occurs {var, term}

    Mismatch {expected, got} -> do
      expected <- applyImpl @t @v expected
      got      <- applyImpl @t @v got
      throw Mismatch {expected, got}
