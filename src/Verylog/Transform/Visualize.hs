{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.Visualize (visualize) where

import qualified Data.Set as S
import Verylog.Language.Types
import Verylog.Solver.FP.Types
import Control.Lens
import Control.Monad.State.Lazy

type Conds = [(Id, Bool)]

data Node a = RegNode { nodeName  :: String
                      , nodeData  :: a
                      }
            | IteNode { nodeName  :: String
                      , nodeData  :: a
                      , nodeConds :: Conds
                      }
            | UFNode  { nodeName  :: String
                      , nodeData  :: a
                      }
            deriving (Eq, Ord)

data Arrow a = Arrow { fromNodes :: [Node a]
                     , toNodes   :: [Node a]
                     , blocking  :: Bool
                     , arrowData :: a
                     }
             deriving (Eq, Ord)

data VisSt a = VisSt { _arrows :: [Arrow a]
                     , _ufMap  :: UFMap
                     , _conds  :: Conds
                     }

makeLenses ''VisSt

visualize :: FPSt -> String
visualize fpst =
  let initSt = VisSt{ _arrows = []
                    , _ufMap  = fpst ^.fpUFs
                    , _conds  = []
                    }
      visSt = evalState (pipeline (fpst ^. fpABs)) initSt
  in printGraph (visSt ^. arrows) 

type V = VisSt ()
type S = State V

pipeline :: [AlwaysBlock] -> S V
pipeline as = do
  sequence_ $ doAB <$> as
  get

doAB :: AlwaysBlock -> S ()
doAB a = doStmt (a^.aStmt)
  where
    doStmt                       :: Stmt -> S ()
    doStmt (Block ss)            = sequence_ $ doStmt <$> ss
    doStmt (BlockingAsgn{..})    = doAsgn True  lhs rhs
    doStmt (NonBlockingAsgn{..}) = doAsgn False lhs rhs
    doStmt (IfStmt{..})          = do currentConds <- use conds
                                      conds .= (:) (ifCond, True) currentConds
                                      doStmt thenStmt
                                      conds .= (:) (ifCond, False) currentConds
                                      doStmt elseStmt
                                      conds .= currentConds
    doStmt Skip                  = return ()

    doAsgn                :: Bool -> Id -> Id -> S ()
    doAsgn isBlocking l r = undefined
     
  

printGraph    :: (Ord a) => [Arrow a] -> String
printGraph as = undefined
  where
    arrowSet = S.fromList as
    nodeSet  = foldr (\v s -> s `S.union`
                              (S.fromList $ fromNodes v) `S.union`
                              (S.fromList $ toNodes v))
               S.empty as
