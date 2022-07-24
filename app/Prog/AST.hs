
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prog.AST where

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
type Type = Term Type_ TName

{- | A carrier functor for type.

     Deriving `Unifiable` gives us freeVars, substitution and unification
     for free.
-}
data Type_ t
  = TCon P TName
  | TArr P [t] t
  | TRec P [(Name, t)]
  | TApp P t t
  deriving stock (Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

type Kind = Term Kind_ KName

data Kind_ k
  = KStar P
  | KArr  P k k
  deriving stock (Eq, Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

kStar :: P -> Kind
kStar = Term . KStar

kArr :: P -> Kind -> Kind -> Kind
kArr p d c = Term $ KArr p d c

newtype KName = KName { unKName :: Name }
  deriving newtype (Eq, Ord, Show, Variable Int)

newtype TName = TName { unTName :: Name }
  deriving newtype (Eq, Ord, Show, Variable Int)

tCon :: P -> String -> Type
tCon p n = Term $ TCon p (TName $ Name p n)

tArr :: P -> [Type] -> Type -> Type
tArr p xs r = Term $ TArr p xs r

tApp :: P -> Type -> Type -> Type
tApp p xs r = Term $ TApp p xs r

tRec :: P -> [(Name, Type)] -> Type
tRec p fs = Term $ TRec p fs

instance Show a => Show (Type_ a) where
  show = \case
    TCon _ n    -> "#" ++ show n
    TArr _ as r -> "(fun " ++ unwords (map show as) ++ " -> " ++ show r ++ ")"
    TRec _ fs   -> "{" ++ intercalate ", " (map show' fs) ++ "}"
    TApp _ f x  -> "(" ++ show f ++ " " ++ show x ++ ")"
   where
    show' (n, t) = show n ++ ": " ++ show t

instance Show a => Show (Kind_ a) where
  show = \case
    KStar _    -> "*"
    KArr _ d c -> "(" ++ show d ++ " -> " ++ show c ++ ")"

deriving anyclass instance Eq a => Unifiable ((,) a)
deriving anyclass instance         Unifiable []
