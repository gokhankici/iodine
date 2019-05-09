module Verylog.Utils ( app2
                     , liftIO1, liftIO2
                     , ifM
                     , while, continue, break, Loop
                     , mapOfSetInsert
                     , silence
                     ) where

import           Prelude                hiding (break)
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Hashable
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           GHC.IO.Handle
import           System.IO

mapOfSetInsert :: (Eq k, Eq v, Hashable k, Hashable v)
               => k -> v -> M.HashMap k (S.HashSet v) -> M.HashMap k (S.HashSet v)
mapOfSetInsert k v = M.alter altr k
  where
    altr Nothing  = Just $ S.singleton v
    altr (Just s) = Just $ S.insert v s

-- -----------------------------------------------------------------------------
-- Monad Stuff
-- -----------------------------------------------------------------------------
app2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
app2 f ma mb = do
  a <- ma
  b <- mb
  f a b

liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO

liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 = ((.).(.)) liftIO

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mc ma1 ma2 = do
  c <- mc
  if c then ma1 else ma2

-- -----------------------------------------------------------------------------
-- Monad Loops
-- -----------------------------------------------------------------------------
data Loop = Break | Continue

while :: Monad m
      => a                      -- the default value.
      -> m Bool                 -- condition monad: Checked each time before running the action.
      -> m (Loop, a)            -- action: If fst is break, loop terminates.
      -> m a                    -- the result of the last action. Or the default if action is never ran.
while aDefault mCondition mAction = do
  c <- mCondition
  if c
    then do (brk, a) <- mAction
            case brk of
              Break    -> return a
              Continue -> while a mCondition mAction
    else return aDefault

-- these are meant to be used with the while function above
continue, break :: Monad m => m a -> m (Loop, a)
continue = fmap ((,) Continue)
break    = fmap ((,) Break)


silence :: IO a -> IO a
silence action = withFile "/dev/null" AppendMode prepareAndRun
  where
    handles = [stdout, stderr]
    prepareAndRun tmpHandle = go handles
      where
        go [] = action
        go hs = goBracket go tmpHandle hs

    goBracket _ _ [] = undefined
    goBracket go tmpHandle (h:hs) = do
      buffering <- hGetBuffering h
      let redirect = do
            old <- hDuplicate h
            hDuplicateTo tmpHandle h
            return old
          restore old = do
            hDuplicateTo old h
            hSetBuffering h buffering
            hClose old
      bracket redirect restore (\_ -> go hs)
