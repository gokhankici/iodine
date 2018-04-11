{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.Visualize (visualize) where

import Verylog.Language.Types
import Verylog.Solver.FP.Types
import Control.Lens
import Control.Monad.State.Lazy

data Node a = RegNode  { nodeName :: String, nodeData :: a }
            | WireNode { nodeName :: String, nodeData :: a } 
            | IteNode  { nodeName :: String, nodeData :: a } 
            | UFNode   { nodeName :: String, nodeData :: a } 

data Arrow a = Arrow { fromNode  :: Node a
                     , toNode    :: Node a
                     , arrowData :: a
                     }

data VisSt a = VisSt { _nodes  :: [Node a]
                     , _arrows :: [Arrow a]
                     , _ufMap  :: UFMap
                     }

makeLenses ''VisSt

visualize :: FPSt -> String
visualize fpst =
  let initSt = VisSt{ _nodes  = []
                    , _arrows = []
                    , _ufMap  = fpst ^.fpUFs
                    }
      visSt = evalState (pipeline (fpst ^. fpABs)) initSt
  in printGraph (visSt ^. nodes) (visSt ^. arrows) 

type V = VisSt ()
type S = State V

pipeline :: [AlwaysBlock] -> S V
pipeline as = do
  get
  

printGraph       :: [Node a] -> [Arrow a] -> String
printGraph ns as = undefined
