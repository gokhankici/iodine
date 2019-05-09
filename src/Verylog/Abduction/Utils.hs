module Verylog.Abduction.Utils where

import Verylog.Utils

import           Control.Monad.State.Lazy
import qualified Data.Sequence       as SQ
import           System.Random

import Debug.Trace

-- | Generate a random value in the monadic context.
randM :: (Random a, MonadIO m) => m a
randM = liftIO randomIO

-- | Run one of the actions randomly and return the result.
chooseM :: MonadIO m => m a -> m a -> m a
chooseM = ifM randM

-- | Pick a random element from the given sequence.
-- The sequence must be non empty, otherwise this throws an error.
randomSample :: MonadIO m => SQ.Seq a -> m a
randomSample sq =
  if   SQ.length sq == 0
  then error "randomSample is called with an empty sequence !"
  else (SQ.index sq) . (`mod` (SQ.length sq)) <$> randM

debug :: String -> a -> a
debug = trace

debugM :: MonadIO m => String -> m ()
debugM = traceM
