
module Control.Unification.Scheme where

import Data.Foldable
import Data.Traversable
import Data.Map qualified as Map
import Control.Monad.State
import Control.Unification.Term

data Scheme t v = Scheme
  { vars :: [v]
  , body :: Term t v
  }

generalise :: (Ord v, Foldable t) => Term t v -> Scheme t v
generalise term = Scheme (toList (allVars term)) term

instantiate
  :: (Ord v, Traversable t, Monad m)
  => (v -> m v)
  -> Scheme t v
  -> m (Term t v)
instantiate rename (Scheme vars body) = do
  vars' <- for vars rename
  let renames = Map.fromList (zip vars vars')
  return $ (renames Map.!) <$> body
