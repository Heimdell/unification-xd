module Main where

import System.Environment (getArgs)

import Debug.Trace

import Control.Monad.Writer (listen, censor, tell)
import Control.Monad.Inference (applyBindings, Context, getContext, monotype)
import Control.Monad.Inference.Carrier (runInferT, UState, Product (..), UState (..))
import Data.Foldable (for_)
import Data.Map qualified as Map

import Ignore
import Pos
import SExpr (parseSExpr)
import Prog (parseProg, inferType, Type_, TName, Kind_, KName (..), kStar, kArr, Name (..))

main :: IO ()
main = do
  getArgs >>= \case
    [file] -> do
      txt <- readFile file
      case parseSExpr txt >>= parseProg of
        Right p -> do
          putStrLn "\n==== Source code ===="
          print p
          putStrLn "\n==== Type ===="

          let
            context :: Product [Context Type_ TName, Context Kind_ KName]
            context
              =  mempty
              :* Map.fromList
                [ (KName (Name (I nowhere) "Int"),    monotype $ kStar (I nowhere))
                , (KName (Name (I nowhere) "String"), monotype $ kStar (I nowhere))
                , (KName (Name (I nowhere) "Unit"),   monotype $ kStar (I nowhere))
                , (KName (Name (I nowhere) "Map"),    monotype $ kArr (I nowhere) (kStar (I nowhere)) (kArr (I nowhere) (kStar (I nowhere)) (kStar (I nowhere))))
                ]
              :* Empty

            state :: Product [UState Type_ TName, UState Kind_ KName, Int]
            state = UState mempty :* UState mempty :* 0 :* Empty

          (ty, s, insts) <- runInferT context state do
            (ty, insts) <-
              censor (const []) do
                listen do
                  inferType p
            ty'    <- applyBindings nowhere ty
            insts' <- (traverse.traverse) (applyBindings nowhere) insts
            tell insts'
            return ty'

          print ty
          putStrLn "\n==== Instantiations ===="
          for_ insts print
          putStrLn "\n==== State ===="
          print s

        Left e -> do
          putStrLn e
    _ -> do
      putStrLn "USAGE: unification-xD <file>"
