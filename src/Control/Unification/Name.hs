
module Control.Unification.Name where

import Polysemy
import Polysemy.State

data Refreshes n m a where
  Refresh :: n -> Refreshes n m n

class IsName n where
  setNameIndex :: Int -> n -> n

makeSem ''Refreshes

runRefreshment
  :: ( Member (State Int) r
     , IsName n
     )
  => Sem (Refreshes n : r) a
  -> Sem                r  a
runRefreshment = interpret \case
  Refresh n -> do
    modify @Int (+ 1)
    flip setNameIndex n <$> get
