module Main where

import System.Environment (getArgs)

import Debug.Trace

import Control.Monad.Writer (listen, censor, tell)
import Control.Monad.Inference (applyBindings, Context, monotype)
import Control.Monad.Inference.Carrier (runInferT, UState, Product (..), UState (..))
import Data.Foldable (for_)
import Data.Map qualified as Map

import Ignore
import Pos
import SExpr (parseSExpr)
import Prog (parseProg, inferType, Type_, KName, Kind_, TName (..), kStar, kArr, Name (..))

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
            star  = kStar (I nowhere)
            arr   = kArr  (I nowhere)
            tname = TName . Name (I nowhere)

            context :: Product [Context Name Type_ TName, Context TName Kind_ KName]
            context
              =  mempty
              :* Map.fromList
                [ (tname "Int",    monotype star)
                , (tname "String", monotype star)
                , (tname "Unit",   monotype star)
                , (tname "Map",    monotype $ star `arr` (star `arr` star))
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
