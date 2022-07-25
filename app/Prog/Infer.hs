
module Prog.Infer where

import Control.Monad.Catch
import Control.Monad.Writer (tell, MonadWriter, join)
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

data UnionError
  = NotACtorType    P Type
  deriving stock    (Show)
  deriving anyclass (Exception)

{- | Infer type of the program.
-}
inferType
  :: ( CanUnify P        Type_ TName Int m
     , CanUnify P        Kind_ KName Int m
     , HasContext   Name Type_ TName m
     , HasContext  TName Kind_ KName m
     , MonadWriter [(Name, Type)] m
     )
  => Prog
  -> m Type
inferType = \case
  V p n -> do
    t <- findVar p n
    -- k <- inferKind t
    -- unify p k (kStar p)
    tell [(n, t)]
    return t

  App p f xs -> do
    tf  <- inferType f
    txs <- traverse inferType xs
    r   <- freshType
    _   <- unify p (tArr p txs r) tf  -- TODO: Check order.
    return r

  Lam p as b -> do
    argTy <- for as \a -> do
      t <- freshType
      return (a, t)

    for_ argTy \n@(Name p' _, t) -> do
      k <- inferKind t
      unify p' k (kStar p')
      return (n, t)

    t <- withMonotypes argTy do  -- Use them while inferring body type.
      inferType b

    return $ tArr p (map snd argTy) t

  Let i ds b -> do
    let nts   = [nt    | TDecl  nt   <- ds]
    let vals  = [val   | Decl  val   <- ds]
    let aliai = [alias | Alias alias <- ds]

    (dks, dts) <- unzip <$> for nts \(Newtype i' n args ctors) -> do
      let ctorReturnTy = foldl (tApp i') (tCon' i' n) (map (Var . TName) args)

      kvars <- traverse (const freshType) args
      let
        deltaKinds = (n, monotype $ foldr (kArr i') (kStar i') kvars)

      deltaTypes <- for ctors \(Ctor i'' n' t) -> do
        let type_ = tArr i'' [t] ctorReturnTy
        qtype <- generalise type_
        return (n', qtype)

      return (deltaKinds, deltaTypes)

    withPolytypes dks do
      withPolytypes (join dts) do
        for_ aliai \(TAlias _ n t) -> do
          TName n =: t

        let ns = [n | Val _ n _ <- vals]

        withFreshMonotypesFor @Type_ @TName ns do

          tys <- for vals \(Val _ n p) -> do    -- Try inferType recursive bindings.
            t <- inferType p
            k <- inferKind t
            unify i k (kStar i)
            return (n, t)

          qtys <- (traverse.traverse) generalise tys

          withPolytypes qtys do         -- Re-push them generalised,
            inferType b                     -- inferType body.

  Int p _ -> return $ tCon p "Int"
  Txt p _ -> return $ tCon p "String"

  Rec p fs -> do
    Compose fTys <- traverse inferType (Compose fs)
    for_ (Compose fTys) \t -> do
      k <- inferKind t
      unify p k (kStar p)
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
          Just t'' -> do
            unify p t'' v'
            return v'

      _ -> throwM $ NonRecordAccess p f t

  Seq _ [p] -> do
    inferType p

  Seq p [] -> do
    return $ tCon p "Unit"

  Seq i (p : ps) -> do
    t <- inferType p
    unify (pInfo p)
      do tCon (I nowhere) "Unit"
      t  -- Unused stuff must be voided.
    inferType (Seq i ps)

  U p -> return $ tCon p "Unit"

  Ann i p t -> do
    t' <- inferType p
    k  <- inferKind t'
    unify i k (kStar i)
    unify i t' t
    return t'

  FFI p t -> do
    k <- inferKind t
    unify p k (kStar p)
    return t

  Map i fs -> do
    tkr <- freshType  -- The empty @(map)@ literal still can get type
    tvr <- freshType  -- from how it is used.

    for_ fs \(k, v) -> do
      tk <- inferType k
      tv <- inferType v
      unify (pInfo k) tkr tk
      unify (pInfo v) tvr tv
      return (tk, tv)

    return $ tApp i (tApp i (tCon i "Map") tkr) tvr

  Mtc _ p alts -> do
    tp  <- inferType p
    res <- freshType
    for_ alts \alt -> do
      tAlt <- inferTypeAlt tp alt
      unify (aInfo alt) res tAlt
    return res

inferTypeAlt
  :: ( CanUnify P        Type_ TName Int m
     , CanUnify P        Kind_ KName Int m
     , HasContext   Name Type_ TName m
     , HasContext  TName Kind_ KName m
     , MonadWriter [(Name, Type)] m
     )
  => Type -> Alt -> m Type
inferTypeAlt t (Alt _ pat p) = do
  bindings <- bind t pat
  withMonotypes bindings do
    inferType p

bind
  :: ( CanUnify P        Type_ TName Int m
     , CanUnify P        Kind_ KName Int m
     , HasContext   Name Type_ TName m
     , HasContext  TName Kind_ KName m
     , MonadWriter [(Name, Type)] m
     )
  => Type -> Pat -> m [(Name, Type)]
bind t = \case
  PVar _ n -> do
    return [(n, t)]

  PCtor i n pat -> do
    t'  <- findVar i n
    t'' <- freshType
    _   <- unify i t' (tArr i [t''] t)
    bind t'' pat

  PRec i fs -> do
    (fs', binds) <- unzip <$> for fs \(n, pat) -> do
      res <- freshType
      binds <- bind res pat
      return ((n, res), binds)

    unify i t (tRec i fs')
    return (join binds)

  PInt i _ -> do
    unify i t (tCon i "Int")
    return []

  PTxt i _ -> do
    unify i t (tCon i "String")
    return []

inferKind
  :: ( CanUnify P Kind_ KName Int m
     , HasContext  TName Kind_ KName m
     )
  => Type
  -> m Kind
inferKind = \case
  Term term -> case term of
    TCon p n -> do
      findVar p n

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
      unify p kf (kArr p kx kr)
      return kr

  Var (TName n) -> do
    return (Var (KName n))
