
{- |
  Unification API.
-}

module Control.Unification (module M) where

import Control.Unification.Error     as M (Error(..))
import Control.Unification.Term      as M (Term(..))
import Control.Unification.Unifiable as M (Unifiable(..))
import Control.Unification.Unify     as M
  ( UnifierState, emptyUnifierState
  , M, runM
  , (=:=)
  , apply
  )