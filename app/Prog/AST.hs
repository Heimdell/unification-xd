
module Prog.AST where

import Control.Monad.RWS
import Data.List (intercalate)
import GHC.Generics

import Control.Monad.Inference (Variable (..), Unifiable, Term(..))

import Pos
import Ignore

{- | The language we are typechecking.

     Fields are there so we can `pInfo` sometimes.
-}
data Prog
  = Let { pInfo :: P, pDecls  :: [Decl], pBody :: Prog }     --
  | App { pInfo :: P, pF      :: Prog,   pXs   :: [Prog] }     --
  | Lam { pInfo :: P, pArgs   :: [Name], pBody :: Prog }    --
  | Int { pInfo :: P, pInt    :: String }          --
  | Txt { pInfo :: P, pStr    :: String }          --
  | Rec { pInfo :: P, pFields :: [(Name, Prog)] } --
  | Get { pInfo :: P, pField  :: Name, pRec   :: Prog }       --
  | Upd { pInfo :: P, pField  :: Name, pValue :: Prog, pRec :: Prog } --
  | Seq { pInfo :: P, pStmts  :: [Prog] }          --
  | U   { pInfo :: P }                 --
  | V   { pInfo :: P, pName   :: Name }            --
  | Ann { pInfo :: P, pProg   :: Prog, pType :: Type }       --
  | FFI { pInfo :: P, pType   :: Type }            --
  | Map { pInfo :: P, pKVs    :: [(Prog, Prog)] }
  deriving stock (Show)

{- | Value declaration.
-}
data Decl
  = Decl  Val
  | Alias TAlias
  deriving stock (Show)

data Val = Val P Name Prog
  deriving stock (Show)

data TAlias = TAlias P Name Type
  deriving stock (Show)

{- | A name. Tracks its origin.
-}
data Name = Name P String
  deriving stock (Eq, Ord)

instance Show Name where
  show (Name _ s) = s

instance Variable Int Name where
  generate i = (Name (I nowhere) $ "_" ++ show i, i + 1)

{- | A type. Uses `Name`s as variables.
-}
type Type = Term Type_ Name

{- | A carrier functor for type.

     Deriving `Unifiable` gives us freeVars, substitution and unification
     for free.
-}
data Type_ t
  = TCon P Name
  | TArr P [t] t
  | TRec P [(Name, t)]
  | TMap P t t
  deriving stock (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

instance Show a => Show (Type_ a) where
  show = \case
    TCon _ n    -> "#" ++ show n
    TArr _ as r -> "(fun " ++ unwords (map show as) ++ " -> " ++ show r ++ ")"
    TRec _ fs   -> "{" ++ intercalate ", " (map show' fs) ++ "}"
    TMap _ k v  -> "(Map " ++ show k ++ " " ++ show v ++ ")"
   where
    show' (n, t) = show n ++ ": " ++ show t

deriving anyclass instance Eq a => Unifiable ((,) a)
deriving anyclass instance         Unifiable []
