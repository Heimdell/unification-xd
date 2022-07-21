
module Pos where

import Ignore

type P = I Pos

{- | Position, to refer back to the source code.
-}
data Pos = Pos
  { lin, col :: Int
  }
  deriving stock (Show)

{- | Initial position in file.
-}
start :: Pos
start = Pos 1 1

{- | Used for generated structures.
-}
nowhere :: Pos
nowhere = Pos (-1) (-1)

{- | Adjust the position by going over 1 char.
-}
advance :: Char -> Pos -> Pos
advance '\n' (Pos l _) = Pos (l + 1) 1
advance  _   (Pos l c) = Pos  l     (c + 1)

{- | Equip all char in the string with positions.
-}
pos :: String -> [(Pos, Char)]
pos = go start
  where
    go p (c : s) = (p, c) : go (advance c p) s
    go _ []      = []
