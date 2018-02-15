{-# language RecordWildCards #-}

module Verylog.Language.Utils where

import           Control.Monad

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
