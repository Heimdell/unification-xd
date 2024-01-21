
module Control.Unification.Scheme where

import Data.Foldable
import Data.Traversable
import Data.Map qualified as Map
import Data.Maybe
import Polysemy
import Polysemy.State
import Control.Unification.Term
import Control.Unification.Name

data Scheme t v = Scheme
  { vars :: [v]
  , body :: t v
  }

generalise :: (Ord v, Foldable t) => t v -> Scheme t v
generalise term = Scheme (toList term) term

instantiate
  :: (Ord v, Traversable t, Member (Refreshes v) r)
  => Scheme t v
  -> Sem r (t v)
instantiate (Scheme vars body) = do
  vars' <- for vars refresh
  let renames = Map.fromList (zip vars vars')
  return $ (\v -> fromMaybe v (Map.lookup v renames)) <$> body
