module Main where

import Control.Monad.RWS
import Control.Monad.Except
import Data.Char
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Traversable (for)
import Data.Foldable (for_)
import GHC.Generics
import Shower

import Fixpoint
import Unification

import System.Environment (getArgs)
import Debug.Trace

{- | Position, to refer back to the source code.
-}
data Pos = Pos
  { lin, col :: Int
  }
  deriving stock (Show)

{- | Initial position in file.
-}
start :: Pos
start = Pos 1 1

{- | Used for generated structures.
-}
nowhere :: Pos
nowhere = Pos (-1) (-1)

{- | Adjust the position by going over 1 char.
-}
advance :: Char -> Pos -> Pos
advance '\n' (Pos l c) = Pos (l + 1) 1
advance  _   (Pos l c) = Pos  l     (c + 1)

{- | Equip all char in the string with positions.
-}
pos :: String -> [(Pos, Char)]
pos s = go start s
  where
    go p (c : s) = (p, c) : go (advance c p) s
    go _ []      = []

{- | We need to hide `Pos` from unifier, or else it will try to do `(==)` on it.
-}
data I a = I { unI :: a }

{- | We need to hide `Pos` from printig as well, because oh my.
-}
instance Show (I a) where show _ = "?"
instance Eq   (I a) where _ == _ = True
instance Ord  (I a) where compare _ _ = EQ

{- | Hidden position.
-}
type P = I Pos

{- | S-expression, because I do not want to write another parser.
-}
data SExpr
  = Sym { sPos :: P, sName  :: String }
  | Lst { sPos :: P, sElems :: [SExpr] }
  | Num { sPos :: P, sNum   :: String }
  | Str { sPos :: P, sStr   :: String }

instance Show SExpr where
  show = \case
    Sym _ s   -> s
    Lst _ els -> "(" ++ unwords (map show els) ++ ")"
    Num _ n   -> n
    Str _ s   -> show s

{- | Hand-written automata. Recognises @(lists)@, @123.4@, @"strings"
     and @symbols@. Does not validate numbers, so
     @1.3131...4@ is accepted as one.

     Does not backtrack.
-}
parseSExpr :: String -> Either String SExpr
parseSExpr = go [(start, [])] . pos
  where
    go                          stack  ((_,  c ) : s) | isSpace c = go stack s
    go                          stack  ((_, ';' ) : s) = go stack $ dropWhile ((/= '\n') . snd) s
    go                          stack  ((p, '(') : s) = go ((p, []) : stack) s
    go ((p, top) : (p', top') : stack) ((_, ')') : s) = go ((p', (Lst (I p) (reverse top) : top')) : stack) s

    go ((p', top) : stack) ((p, '\'') : s) = do
      case break ((== '\'') . snd) s of
        (str, _ : s') -> go ((p', Str (I p) (map snd str) : top) : stack) s'
        _             -> Left $ "unclosed string literal, open at " ++ show p

    go ((p', top) : stack) ((p, c) : s)
      | isDigit c = do
        case break ((\c -> not (or [isDigit c, c == '.'])) . snd) s of
          (str, s') -> go ((p', Num (I p) (c : map snd str) : top) : stack) s'

      | not (c `elem` " \n()'") = do
        case break ((\c -> c `elem` " \n()'") . snd) s of
          (str, s') -> go ((p', Sym (I p) (c : map snd str) : top) : stack) s'

    go [(p, top)] [] = return $ Lst (I p) (reverse top)

    go stack rest@((p, _) : _) = Left $ "in state " ++ show stack ++ " unexpected at " ++ show p ++ " <" ++ map snd rest ++ ">"
    go stack []                = Left $ "in state " ++ show stack ++ " unexpected EOF"

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
data Decl = Decl P Name Prog
  deriving stock (Show)

{- | A name. Tracks its origin.
-}
data Name = Name P String
  deriving stock (Show, Eq, Ord)

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
  deriving stock (Show, Functor, Foldable, Traversable, Generic1)
  deriving anyclass (Unifiable)

deriving anyclass instance Eq a => Unifiable ((,) a)
deriving anyclass instance         Unifiable []

{- | Parse prog from `SExpr`.
-}
parseProg :: SExpr -> Either String Prog
parseProg = \case
  Lst p [Sym _ "let", Lst _ decls, body] -> do
    b <- parseProg body
    ds <- traverse parseDecls decls
    return $ Let p ds b

  Lst p [Sym _ "fun", Lst _ args, body] -> do
    as <- traverse parseName args
    b  <- parseProg body
    return $ Lam p as b

  Lst p (Sym _ "begin" : progs) -> do
    ps <- traverse parseProg progs
    return $ Seq p ps

  Lst p [Sym _ "get", field, rec] -> do
    f <- parseName field
    r <- parseProg rec
    return $ Get p f r

  Lst p [Sym _ "set", field, val, rec] -> do
    f <- parseName field
    r <- parseProg rec
    v <- parseProg val
    return $ Upd p f v r

  Lst p [Sym _ "::", prog, ty] -> do
    b <- parseProg prog
    t <- parseType ty
    return $ Ann p b t

  Lst p [Sym _ "ffi", ty] -> do
    t <- parseType ty
    return $ FFI p t

  Lst p (Sym _ "rec" : fields) -> do
    fs <- traverse parseFields fields
    return $ Rec p fs

  Lst p (Sym _ "map" : pairs) -> do
    fs <- traverse parsePairs pairs
    return $ Map p fs

  Lst p [] -> do
    return $ U p

  Num p n -> do
    return $ Int p n

  Str p n -> do
    return $ Txt p n

  s@(Sym p n) -> do
    n <- parseName s
    return $ V p n

  Lst p (f : xs) -> do
    f  <- parseProg f
    xs <- traverse parseProg xs
    return $ App p f xs

parsePairs :: SExpr -> Either String (Prog, Prog)
parsePairs = \case
  Lst p [name, prog] -> do
    n <- parseProg name
    p <- parseProg prog
    return (n, p)
  s -> Left $ "expected (name ty) at " ++ show (unI (sPos s))

parseFields :: SExpr -> Either String (Name, Prog)
parseFields = \case
  Lst p [name, prog] -> do
    n <- parseName name
    p <- parseProg prog
    return (n, p)
  s -> Left $ "expected (name ty) at " ++ show (unI (sPos s))

parseType :: SExpr -> Either String Type
parseType = \case
  Lst p [Sym _ "fun", Lst _ tyArgs, ty] -> do
    as <- traverse parseType tyArgs
    t  <- parseType ty
    return $ Term $ TArr p as t

  Lst p [Sym _ "map", k, v] -> do
    k  <- parseType k
    v  <- parseType v
    return $ Term $ TMap p k v

  Lst p (Sym _ "rec" : fields) -> do
    fs <- traverse parseTypeFields fields
    return $ Term $ TRec p fs

  s@(Sym p n@(c : _))
    | isUpper c -> do
      n <- parseName s
      return $ Term $ TCon p n

    | otherwise -> do
      n <- parseName s
      return $ Var n
  s -> Left $ "expected type at " ++ show (unI (sPos s))


parseTypeFields :: SExpr -> Either String (Name, Type)
parseTypeFields = \case
  Lst p [name, ty] -> do
    n <- parseName name
    t <- parseType ty
    return (n, t)
  s -> Left $ "expected (name ty) at " ++ show (unI (sPos s))

parseDecls :: SExpr -> Either String Decl
parseDecls = \case
  Lst p [x, body] -> do
    n <- parseName x
    b <- parseProg body
    return $ Decl p n b
  s -> do
    Left $ "expected (name prog) at " ++ show (unI (sPos s))

parseName :: SExpr -> Either String Name
parseName = \case
  Sym p n -> return $ Name p n
  s       -> Left $ "expected name at " ++ show (unI (sPos s))

{- | Various events that can stop the inference process.

     Each of them points to a source location.
-}
data CompilerError
  = Undefined       Pos Name                -- ^ Variable was not declared.
  | NoField         Pos Name Type           -- ^ There is no such field in the record.
  | NonRecordAccess Pos Name Type           -- ^ Field access on non-record.
  | Unification     Pos (UError Type_ Name) -- ^ Equip unification error with position.
  deriving stock (Show)

{- | The @mtl@ moment, again. On other hand, @mtl@ forces us to handle `UError`.
-}
type UnifyM = UnificationT Type_ Name (RWS Context () Int)
type InferM = ExceptT CompilerError UnifyM

{- | Types are stored as Rank1-qualified.
-}
data QType = Forall { qtArgs :: [Name], qtType :: Type }
  deriving stock (Show)

{- | @Var : Type@ relation.
-}
type Context = Map Name QType

instance Monoid b => MonadRefresh (RWS a b Int) Name where
  fresh = do
    modify (+1)
    i <- get
    return $ Name (I nowhere) $ "_" ++ show i

{- | Run the machinery, dump all the gust on the floor.
-}
runInferM :: InferM a -> (Either (UError Type_ Name) (Either CompilerError a, UState Type_ Name), Int, ())
runInferM act = runRWS (runUnification $ runExceptT act) Map.empty 0

{- | Handle `UError`s by equipping them with `Pos`.
-}
rethrow :: Pos -> UnifyM a -> InferM a
rethrow p act = do
  ea <- lift do tryError act
  case ea of
    Left ue -> throwError $ Unification p ue
    Right a -> return a
  where
    tryError :: MonadError e m => m a -> m (Either e a)
    tryError action = (Right <$> action) `catchError` (pure . Left)

{- | Infer type of the program.
-}
infer :: Prog -> InferM Type
infer = \case
  V (I p) n -> do
    asks (Map.lookup n) >>= \case
      Nothing -> throwError $ Undefined p n
      Just (Forall ns t) -> do
        t <- rethrow p do instantiate ns t  -- Variables have forall-types.
        return t

  App (I p) f xs -> do
    tf  <- infer f
    txs <- traverse infer xs
    r   <- Var <$> fresh
    rethrow p do unify (Term $ TArr (I p) txs r) tf  -- TODO: Check order.
    return r

  Lam (I p) as b -> do
    delta <- for as \a -> do
      t <- Var <$> fresh                   -- Invent new monotypes for args.
      return (a, Forall [] t)

    t <- local (Map.fromList delta <>) do  -- Use them while inferring body type.
      infer b

    let tys = map (qtType . snd) delta

    return $ Term $ TArr (I p) tys t

  Let (I p) ds b -> do
    delta <- for ds \(Decl _ n p) -> do     -- TODO: is it correct?
      t <- Var <$> fresh                    -- Assign fresh monotypes to all funs
      return (n, Forall [] t, p)            -- in recursive group.

    let d = [(n, qt) | (n, qt, _) <- delta]

    local (Map.fromList d <>) do
      tys <- for delta \(n, qt, p) -> do    -- Try infer recursive bindings.
        infer p

      qtys <- for tys \t -> do              -- /After that/, generalise them.
        ns <- getFreeVars t
        return $ Forall ns t

      let d' = [(n, qt) | ((n, _), qt) <- zip d qtys]

      local (Map.fromList d' <>) do         -- Re-push them generalised,
        infer b                             -- infer body.

  Int p _ -> return $ Term $ TCon p $ Name p "Int"
  Txt p _ -> return $ Term $ TCon p $ Name p "String"

  Rec p fs -> do
    fTys <- for fs \(f, p) -> do
      t <- infer p
      return (f, t)

    return $ Term $ TRec p fTys

  Get (I p) f b -> do
    t <- infer b
    t <- rethrow p do applyBindings t           -- We have to, we can't unify
    case t of                                   -- with record of arbitrary size.
      Term (TRec _ fTys) -> do                  -- The record /structure/ must be already known.
        case lookup f fTys of                   -- (it still may contain variables!)
          Nothing -> throwError $ NoField p f t
          Just t  -> return t

      _ -> throwError $ NonRecordAccess p f t

  Upd (I p) f v b -> do
    t <- infer b
    v <- infer v
    t <- rethrow p do applyBindings t          -- Same as in `Get`.
    case t of
      Term (TRec _ fTys) -> do
        case lookup f fTys of
          Nothing -> throwError $ NoField p f t
          Just t  -> rethrow p do unify t v

      _ -> throwError $ NonRecordAccess p f t

  Seq _ [p] -> do
    infer p

  Seq i (p : ps) -> do
    t <- infer p
    rethrow (unI $ pInfo p) do
      unify (Term $ TCon (I nowhere) $ Name (I nowhere) "Unit") t  -- Unused stuff must be voided.
    infer (Seq i ps)

  U p -> return $ Term $ TCon p $ Name p "Unit"

  Ann (I i) p t -> do
    t' <- infer p
    rethrow i do unify t' t

  FFI i t -> do
    return t

  Map i fs -> do
    tkr <- Var <$> fresh  -- The empty @(map)@ literal still can get type
    tvr <- Var <$> fresh  -- from how it is used.

    for_ fs \(k, v) -> do
      tk <- infer k
      tv <- infer v
      rethrow (unI $ pInfo k) do unify tkr tk
      rethrow (unI $ pInfo v) do unify tvr tv
      return (tk, tv)

    return $ Term $ TMap i tkr tvr

{-
data Prog
  | V   P Name            --
  = Let P [Decl] Prog     --
  | App P Prog [Prog]     --
  | Lam P [Name] Prog     --
  | Int P String          --
  | Txt P String          --
  | Rec P [(Name, Prog)]  --
  | Get P Name Prog       --
  | Upd P Name Prog Prog  --
  | Seq P [Prog]          --
  | U   P                 --
  | Ann P Prog Type       --
-}

main :: IO ()
main = do
  getArgs >>= \case
    [file] -> do
      txt <- readFile file
      case parseSExpr txt >>= parseProg of
        Right (App _ p []) -> do
          putStrLn "\n==== Source code ===="
          printer p
          putStrLn "\n==== Guts of type inference ===="
          printer $ runInferM do infer p >>= lift . applyBindings

        Left e -> do
          putStrLn e