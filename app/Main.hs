module Main where

import Control.Monad.Writer
import System.Environment (getArgs)

import Shower

import Control.Monad.Inference (applyBindings)
import Control.Monad.Inference.Carrier (runRefreshT, runInferT)

import Pos
import SExpr (parseSExpr)
import Prog (parseProg, infer, Name)

main :: IO ()
main = do
  getArgs >>= \case
    [file] -> do
      txt <- readFile file
      case parseSExpr txt >>= parseProg of
        Right p -> do
          putStrLn "\n==== Source code ===="
          printer p
          putStrLn "\n==== Type ===="
          (((ty, insts), s), _) <- runRefreshT @_ @Name 0 do
            runWriterT do
              runInferT mempty do
                (ty, insts) <- listen do infer p
                ty'    <- applyBindings nowhere ty
                insts' <- (traverse.traverse) (applyBindings nowhere) insts
                return (ty', insts')

          printer ty
          putStrLn "\n==== Instantiations ===="
          printer insts
          putStrLn "\n==== State ===="
          printer s



        Left e -> do
          putStrLn e
    _ -> do
      putStrLn "USAGE: unification-xD <file>"
