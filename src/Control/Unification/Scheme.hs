
module Control.Unification.Scheme where

import Data.Foldable
import Data.Traversable
import Data.Map qualified as Map
import Polysemy
import Polysemy.State
import Control.Unification.Term

data Scheme t v = Scheme
  { vars :: [v]
  , body :: t v
  }

generalise :: (Ord v, Foldable t) => t v -> Scheme t v
generalise term = Scheme (toList term) term

data Refreshes n m a where
  Refresh :: n -> Refreshes n m n

makeSem ''Refreshes

instantiate
  :: (Ord v, Traversable t, Member (Refreshes v) r)
  => Scheme t v
  -> Sem r (t v)
instantiate (Scheme vars body) = do
  vars' <- for vars refresh
  let renames = Map.fromList (zip vars vars')
  return $ (renames Map.!) <$> body
