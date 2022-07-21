
module Prog.Infer where

import Control.Monad.Catch
import Control.Monad.Writer
import Data.Functor.Compose
import Data.Foldable (for_)
import Data.Traversable (for)

import Control.Monad.Inference

import Ignore
import Pos
import Prog.AST

import Debug.Trace
import Shower

data RecordError
  = NoField         P Name Type           -- ^ There is no such field in the record.
  | NonRecordAccess P Name Type           -- ^ Field access on non-record.
  deriving stock    (Show)
  deriving anyclass (Exception)

{- | Infer type of the program.
-}
infer
  :: ( CanInfer P Type_ Name Int m
     , MonadWriter [(Name, Type)] m
     )
  => Prog
  -> m Type
infer = \case
  V p n -> do
    t <- findVar p n
    tell [(n, t)]
    return t

  App p f xs -> do
    tf  <- infer f
    txs <- traverse infer xs
    r   <- freshType
    unify p (Term $ TArr p txs r) tf  -- TODO: Check order.
    return r

  Lam p as b -> do
    argTy <- for as \_ -> freshType

    t <- withMonotypes (zip as argTy) do  -- Use them while inferring body type.
      infer b

    return $ Term $ TArr p argTy t

  Let p ds b -> do
    let vals  = [val   | Decl  val   <- ds]
    let aliai = [alias | Alias alias <- ds]

    for_ aliai \(TAlias _ n t) -> do
      n =: t

    let ns = [n | Val _ n _ <- vals]

    withFreshMonotypesFor ns do

      tys <- for vals \v@(Val _ _ p) -> do    -- Try infer recursive bindings.
        t <- infer p
        return (v, t)

      qtys <- for tys \(Val i n _, t) -> do
        qt <- generalise i t
        return (n, qt)

      withPolytypes qtys do         -- Re-push them generalised,
        infer b                             -- infer body.

  Int p _ -> return $ Term $ TCon p $ Name p "Int"
  Txt p _ -> return $ Term $ TCon p $ Name p "String"

  Rec p fs -> do
    Compose fTys <- traverse infer (Compose fs)
    return $ Term $ TRec p fTys

  Get p f b -> do
    t <- infer b
    t <- applyBindings p t           -- We have to, we can't unify
    case t of                                   -- with record of arbitrary size.
      Term (TRec _ fTys) -> do                  -- The record /structure/ must be already known.
        case lookup f fTys of                   -- (it still may contain variables!)
          Nothing -> throwM $ NoField p f t
          Just t  -> return t

      _ -> throwM $ NonRecordAccess p f t

  Upd p f v b -> do
    t <- infer b
    v <- infer v
    t <- applyBindings p t          -- Same as in `Get`.
    case t of
      Term (TRec _ fTys) -> do
        case lookup f fTys of
          Nothing -> throwM $ NoField p f t
          Just t  -> unify p t v

      _ -> throwM $ NonRecordAccess p f t

  Seq _ [p] -> do
    infer p

  Seq i (p : ps) -> do
    t <- infer p
    unify (pInfo p)
      do Term $ TCon (I nowhere) $ Name (I nowhere) "Unit"
      t  -- Unused stuff must be voided.
    infer (Seq i ps)

  U p -> return $ Term $ TCon p $ Name p "Unit"

  Ann i p t -> do
    t' <- infer p
    unify i t' t

  FFI i t -> do
    return t

  Map i fs -> do
    tkr <- freshType  -- The empty @(map)@ literal still can get type
    tvr <- freshType  -- from how it is used.

    for_ fs \(k, v) -> do
      tk <- infer k
      tv <- infer v
      unify (pInfo k) tkr tk
      unify (pInfo v) tvr tv
      return (tk, tv)

    return $ Term $ TMap i tkr tvr
