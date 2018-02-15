{-# language RecordWildCards #-}

module Verylog.Language.Utils where

import           Control.Monad

import           Verylog.Language.Types

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) = liftM2 (||)

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)

isGate                :: IR -> Bool
isGate (ContAsgn{..}) = True
isGate _              = False

isProcess              :: IR -> Bool
isProcess (Always{..}) = True
isProcess _            = False

safeHead :: a -> [a] -> a
safeHead def []    = def
safeHead _   (x:_) = x

getRange :: Int -> Int -> [a] -> [a]
getRange i1 i2
  = take (i2 - i1 + 1)
  . drop (i1 - 1)
