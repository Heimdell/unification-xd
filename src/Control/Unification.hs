
{- |
  Unification API.
-}

module Control.Unification (module M) where

import Control.Unification.Error     as M (Mismatch(..), Occurs(..))
import Control.Unification.Term      as M (Term(..))
import Control.Unification.Unifiable as M (Unifiable(..))
import Control.Unification.Unify     as M
  ( UnifierState, emptyUnifierState
  , (=:=)
  , apply
  , save
  , restore
  , Unifies
  , runUnification
  )
import Control.Unification.Scheme    as M
  ( Scheme(..)
  , instantiate
  , generalise
  )

import Control.Unification.Name    as M
  ( Refreshes
  , runRefreshment
  , IsName(..)
  , refresh
  )
