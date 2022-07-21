
module SExpr where

import Data.Char (isDigit, isSpace)

import Pos
import Ignore

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
