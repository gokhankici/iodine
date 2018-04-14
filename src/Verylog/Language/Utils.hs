{-# language RecordWildCards #-}

module Verylog.Language.Utils where

import           Control.Monad
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S
import           Data.Hashable

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) = liftM2 (||)

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)

safeHead :: a -> [a] -> a
safeHead def []    = def
safeHead _   (x:_) = x

getRange :: Int -> Int -> [a] -> [a]
getRange i1 i2
  = take (i2 - i1 + 1)
  . drop (i1 - 1)

mapOfSetInsert :: (Eq k, Eq v, Hashable k, Hashable v)
               => k -> v -> M.HashMap k (S.HashSet v) -> M.HashMap k (S.HashSet v)
mapOfSetInsert k v m = M.alter altr k m
  where
    altr Nothing  = Just $ S.singleton v
    altr (Just s) = Just $ S.insert v s
