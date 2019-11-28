module Iodine.Utils where

import           Iodine.Language.Types
import           Data.Foldable
import qualified Data.HashSet as HS

combine :: (Monad f, Monoid m, Traversable t) => (a -> f m) -> t a -> f m
combine act as = foldl' (<>) mempty <$> traverse act as

mfold :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
mfold f = foldl' (\ms a -> f a <> ms) mempty

intersects :: HS.HashSet Id -> HS.HashSet Id -> Bool
intersects s1 s2 = go (HS.toList s1)
 where
  go []       = False
  go (a : as) = HS.member a s2 || go as

not_supported :: a
not_supported = error "not supported"
