{- | Common unification errors.
-}

module Control.Monad.Inference.Errors where

import Control.Monad.Catch
import Data.Typeable (Typeable)

import Data.Functor.Fixpoints

{- | Unification error.
-}
data UError pos term var
  = {- | Variable is bound to var type that refers to the variable itself.

         We don'term support cyclic or recursive types.
    -}
    Occurs pos var (Term term var)

  | {- | We expected one type, but got something else.
    -}
    Mismatch pos (Term term var) (Term term var)

deriving stock    instance CanShowUError  pos term var => Show      (UError pos term var)
deriving anyclass instance CanThrowUError pos term var => Exception (UError pos term var)

{- | `CanShowUError` + everything is `Typeable`.
-}
type CanThrowUError pos term var =
  ( Typeable var
  , Typeable pos
  , Typeable term, CanShowUError pos term var
  )

{- | Everyhting is `Show`.
-}
type CanShowUError pos term var =
  ( Show var
  , Show pos
  , Show (term Unshow), Functor term
  )
