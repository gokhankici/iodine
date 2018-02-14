{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.InitialPass ( initialPass
                                     ) where

import Control.Lens
import Data.Set
import Control.Monad.State.Lazy

import Verylog.Language.Types

data St = St { _registers :: Set Id
             , _wires     :: Set Id
             , _ufs       :: Set Id
             , _irs       :: [IR]
             }

makeLenses ''St

initialPass       :: [IR] -> [IR]
initialPass input = evalState pipeline initialSt
  where
    initialSt = St empty empty empty input
    pipeline  = collectVars >> lastpass
    lastpass  = use irs

collectVars :: State St ()
collectVars = do uses irs $ flip forM_ collectVar
                 return ()

collectVar :: IR -> State St ()
collectVar (Register id) = registers %= insert id
collectVar (Wire id)     = wires     %= insert id
collectVar (UF id _)     = ufs       %= insert id
collectVar _             = return ()
