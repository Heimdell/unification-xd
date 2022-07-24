
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

data RecordError
  = NoField         P Name Type           -- ^ There is no such field in the record.
  | NonRecordAccess P Name Type           -- ^ Field access on non-record.
  deriving stock    (Show)
  deriving anyclass (Exception)

{- | Infer type of the program.
-}
inferType
  :: ( CanInfer P Type_ TName Int m
     , CanInfer P Kind_ KName Int m
     , MonadWriter [(TName, Type)] m
     )
  => Prog
  -> m Type
inferType = \case
  V p n -> do
    t <- findVar p (TName n)
    k <- inferKind t
    _ <- unify p k (kStar p)
    tell [(TName n, t)]
    return t

  App p f xs -> do
    tf  <- inferType f
    txs <- traverse inferType xs
    r   <- freshType
    _   <- unify p (tArr p txs r) tf  -- TODO: Check order.
    return r

  Lam p as b -> do
    argTy <- for as \_ -> freshType
    for_ argTy \t -> do
      k <- inferKind t
      _ <- unify p k (kStar p)
      return ()

    t <- withMonotypes (zip (map TName as) argTy) do  -- Use them while inferring body type.
      inferType b

    return $ tArr p argTy t

  Let i ds b -> do
    let vals  = [val   | Decl  val   <- ds]
    let aliai = [alias | Alias alias <- ds]

    for_ aliai \(TAlias _ n t) -> do
      TName n =: t

    let ns = [n | Val _ n _ <- vals]

    withFreshMonotypesFor @Type_ (map TName ns) do

      tys <- for vals \(Val _ n p) -> do    -- Try inferType recursive bindings.
        t <- inferType p
        k <- inferKind t
        _ <- unify i k (kStar i)
        return (TName n, t)

      qtys <- (traverse.traverse) generalise tys

      withPolytypes qtys do         -- Re-push them generalised,
        inferType b                     -- inferType body.

  Int p _ -> return $ tCon p "Int"
  Txt p _ -> return $ tCon p "String"

  Rec p fs -> do
    Compose fTys <- traverse inferType (Compose fs)
    for_ (Compose fTys) \t -> do
      k <- inferKind t
      _ <- unify p k (kStar p)
      return ()
    return $ tRec p fTys

  Get p f b -> do
    t  <- inferType b
    t' <- applyBindings p t                     -- We have to, we can't unify
    case t' of                                   -- with record of arbitrary size.
      Term (TRec _ fTys) -> do                  -- The record /structure/ must be already known.
        case lookup f fTys of                   -- (it still may contain variables!)
          Nothing  -> throwM $ NoField p f t
          Just t'' -> return t''

      _ -> throwM $ NonRecordAccess p f t'

  Upd p f v b -> do
    t  <- inferType b
    v' <- inferType v
    t' <- applyBindings p t          -- Same as in `Get`.
    case t' of
      Term (TRec _ fTys) -> do
        case lookup f fTys of
          Nothing  -> throwM $ NoField p f t'
          Just t'' -> unify p t'' v'

      _ -> throwM $ NonRecordAccess p f t

  Seq _ [p] -> do
    inferType p

  Seq p [] -> do
    return $ tCon p "Unit"

  Seq i (p : ps) -> do
    t <- inferType p
    _ <- unify (pInfo p)
      do tCon (I nowhere) "Unit"
      t  -- Unused stuff must be voided.
    inferType (Seq i ps)

  U p -> return $ tCon p "Unit"

  Ann i p t -> do
    t' <- inferType p
    unify i t' t

  FFI _ t -> do
    return t

  Map i fs -> do
    tkr <- freshType  -- The empty @(map)@ literal still can get type
    tvr <- freshType  -- from how it is used.

    for_ fs \(k, v) -> do
      tk <- inferType k
      tv <- inferType v
      _  <- unify (pInfo k) tkr tk
      _  <- unify (pInfo v) tvr tv
      return (tk, tv)

    return $ tApp i (tApp i (tCon i "Map") tkr) tvr

inferKind
  :: ( CanInfer P Kind_ KName Int m
     , CanInfer P Type_ TName Int m
     )
  => Type
  -> m Kind
inferKind = \case
  Term term -> case term of
    TCon p (TName n) -> do
      findVar p (KName n)

    TRec p fs -> do
      for_ fs \(_, t) -> do
        k <- inferKind t
        unify p k (kStar p)
      return (kStar p)

    TArr p xs r -> do
      for_ (r : xs) \t -> do
        k <- inferKind t
        unify p k (kStar p)
      return (kStar p)

    TApp p f x -> do
      kf <- inferKind f
      kx <- inferKind x
      kr <- freshType
      _ <- unify p kf (kArr p kx kr)
      return kr

  Var (TName n) -> do
    return (Var (KName n))
