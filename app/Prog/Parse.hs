
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
    return $ Term $ TArr p as t

  Lst p [Sym _ "map", k, v] -> do
    k' <- parseType k
    v' <- parseType v
    return $ Term $ TMap p k' v'

  Lst p (Sym _ "rec" : fields) -> do
    fs <- traverse parseTypeFields fields
    return $ Term $ TRec p fs

  Sym p ('#' : n) -> do
    n' <- parseName (Sym p n)
    return $ Term $ TCon p n'

  s@Sym {} -> do
    n <- parseName s
    return $ Var n

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
  Lst p [x, body] -> do
    n <- parseName x
    b <- parseProg body
    return $ Decl $ Val p n b
  Lst p [Sym _ "type", n, ty] -> do
    n'  <- parseName n
    ty' <- parseType ty
    return $ Alias $ TAlias p n' ty'
  s -> do
    Left $ "expected (name prog) at " ++ show (unI (sPos s))

parseName :: SExpr -> Either String Name
parseName = \case
  Sym p n -> return $ Name p n
  s       -> Left $ "expected name at " ++ show (unI (sPos s))
