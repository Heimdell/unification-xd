
module Prog.Parse (parseProg) where

import Control.Monad.Inference (Term (..))

import Ignore
import SExpr
import Prog.AST

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

  Lst i (Sym _ "case" : p : alts) -> do
    p'    <- parseProg p
    alts' <- traverse parseAlt alts
    return $ Mtc i p' alts'

  Lst p [] -> do
    return $ U p

  Num p n -> do
    return $ Int p n

  Str p n -> do
    return $ Txt p n

  s@(Sym p _) -> do
    n <- parseName s
    return $ V p n

  Lst p (f : xs) -> do
    f'  <- parseProg f
    xs' <- traverse parseProg xs
    return $ App p f' xs'

parseAlt :: SExpr -> Either String Alt
parseAlt = \case
  Lst i [pat, prog] -> do
    pat'  <- parsePat  pat
    prog' <- parseProg prog
    return $ Alt i pat' prog'
  s -> Left $ "expected (pat prog) at " ++ show (unI (sPos s))

parsePat :: SExpr -> Either String Pat
parsePat = \case
  n@(Sym i _) -> do
    n' <- parseName n
    return $ PVar i n'

  Lst i (Sym _ "rec" : patFields) -> do
    patFields' <- traverse parsePatFields patFields
    return $ PRec i patFields'

  Lst i [n, pat] -> do
    n'   <- parseName n
    pat' <- parsePat  pat
    return $ PCtor i n' pat'

  Num p n -> do
    return $ PInt p n

  Str p n -> do
    return $ PTxt p n

  s -> Left $ "expected pat at " ++ show (unI (sPos s))

parsePatFields :: SExpr -> Either String (Name, Pat)
parsePatFields = \case
  Lst _ [n, pat] -> do
    n'   <- parseName n
    pat' <- parsePat  pat
    return (n', pat')

  n@Sym {} -> do
    n'   <- parseName n
    pat' <- parsePat n
    return (n', pat')

  s -> Left $ "expected (name pat) or name at " ++ show (unI (sPos s))

parsePairs :: SExpr -> Either String (Prog, Prog)
parsePairs = \case
  Lst _ [name, prog] -> do
    n <- parseProg name
    p <- parseProg prog
    return (n, p)
  s -> Left $ "expected (name ty) at " ++ show (unI (sPos s))

parseFields :: SExpr -> Either String (Name, Prog)
parseFields = \case
  Lst _ [name, prog] -> do
    n <- parseName name
    p <- parseProg prog
    return (n, p)
  s -> Left $ "expected (name ty) at " ++ show (unI (sPos s))

parseType :: SExpr -> Either String Type
parseType = \case
  Lst p [Sym _ "fun", Lst _ tyArgs, ty] -> do
    as <- traverse parseType tyArgs
    t  <- parseType ty
    return $ tArr p as t

  Lst p (Sym _ "rec" : fields) -> do
    fs <- traverse parseTypeFields fields
    return $ Term $ TRec p fs

  Lst p (f : xs) -> do
    f'  <- parseType f
    xs' <- traverse parseType xs
    return $ foldl (tApp p) f' xs'

  Sym p ('#' : n) -> do
    return $ tCon p n

  s@Sym {} -> do
    n <- parseName s
    return $ Var $ TName n

  s -> Left $ "expected type at " ++ show (unI (sPos s))

parseTypeFields :: SExpr -> Either String (Name, Type)
parseTypeFields = \case
  Lst _ [name, ty] -> do
    n <- parseName name
    t <- parseType ty
    return (n, t)
  s -> Left $ "expected (name ty) at " ++ show (unI (sPos s))

parseDecls :: SExpr -> Either String Decl
parseDecls = \case
  Lst p [Sym _ "type", n, ty] -> do
    n'  <- parseName n
    ty' <- parseType ty
    return $ Alias $ TAlias p n' ty'
  Lst p [Sym _ "newtype", Sym p' ('#':n'), Lst _ args, Lst _ ctors] -> do
    n''    <- parseName (Sym p' n')
    args'  <- traverse parseName args
    ctors' <- traverse parseCtor ctors
    return $ TDecl $ Newtype p (TName n'') args' ctors'
  Lst p [x, body] -> do
    n <- parseName x
    b <- parseProg body
    return $ Decl $ Val p n b
  s -> do
    Left $ "expected (name prog) at " ++ show (unI (sPos s)) ++ ", not " ++ show s

parseCtor :: SExpr -> Either String Ctor
parseCtor = \case
  Lst p [n, t] -> do
    n' <- parseName n
    t' <- parseType t
    return $ Ctor p n' t'
  s -> do
    Left $ "expected (name type) at " ++ show (unI (sPos s))

parseName :: SExpr -> Either String Name
parseName = \case
  Sym p n -> return $ Name p n
  s       -> Left $ "expected name at " ++ show (unI (sPos s))
