module Main where

import Control.Monad.Catch
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Either
import Data.Foldable (for_)
import Data.Functor.Compose
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import GHC.Generics
import System.Environment (getArgs)

import Shower
import Debug.Trace

import Control.Monad.Inference (applyBindings)
import Control.Monad.Inference.Carrier (runRefreshT, runInferT)

import Pos
import Ignore
import SExpr (parseSExpr)
import Prog (parseProg, infer, Prog(App), Name)

main :: IO ()
main = do
  getArgs >>= \case
    [file] -> do
      txt <- readFile file
      case parseSExpr txt >>= parseProg of
        Right (App _ p []) -> do
          putStrLn "\n==== Source code ===="
          printer p
          putStrLn "\n==== Type ===="
          (((ty, insts), s), _) <- runRefreshT @_ @Name 0 do
            runWriterT do
              runInferT mempty do
                (ty, insts) <- listen do infer p
                ty    <- applyBindings nowhere ty
                insts <- (traverse.traverse) (applyBindings nowhere) insts
                return (ty, insts)

          printer ty
          putStrLn "\n==== Instantiations ===="
          printer insts
          putStrLn "\n==== State ===="
          printer s

        Left e -> do
          putStrLn e
