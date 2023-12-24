
module Control.Unification.Scheme where

import Data.Foldable
import Data.Traversable
import Data.Map qualified as Map
import Control.Monad.State
import Control.Unification.Term

data Scheme t v = Scheme
  { vars :: [v]
  , body :: t v
  }

generalise :: (Ord v, Foldable t) => t v -> Scheme t v
generalise term = Scheme (toList term) term

class Monad m => CanRefresh m n where
  refresh :: n -> m n

instantiate
  :: (Ord v, Traversable t, CanRefresh m v)
  => Scheme t v
  -> m (t v)
instantiate (Scheme vars body) = do
  vars' <- for vars refresh
  let renames = Map.fromList (zip vars vars')
  return $ (renames Map.!) <$> body
