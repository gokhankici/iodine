{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Iodine.Analyze.ModuleDependency
 (
   topsortModules
 )
where

import           Iodine.Language.IR
import           Iodine.Types

import           Control.Lens
import           Data.Foldable
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query as GQ
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import           Polysemy
import           Polysemy.State

type DepGraph = Gr () ()

data St =
  St { _depGraph      :: DepGraph
     , _moduleMap     :: HM.HashMap Id Int
     , _moduleCounter :: Int
     }

makeLenses ''St


{- |
Sort the given modules such that if a module m1 depends on m2, m2 appears
earlier than m1 in the result.
-}
topsortModules :: Foldable t => t (Module a) -> L (Module a)
topsortModules modules =
  foldl' (\ms n -> ms |> moduleNameMap HM.! (moduleNodes IM.! n)) mempty ts
  where
    ts = GQ.topsort g
    (g, moduleNodes) = usedByGraph modules
    moduleNameMap =
      foldl' (\acc m@Module{..} -> HM.insert moduleName m acc) mempty modules


{- |
Creates an used-by graph for modules: (m1 --> m2) is an edge of the graph iff
module m2 instantiates m1 (or m1 is used by m2). Function also returns a mapping
between the node ids and module names.
-}
usedByGraph :: Foldable t
            => t (Module a)
            -> (DepGraph, IM.IntMap Id)
usedByGraph modules = (st ^. depGraph, moduleNodes)
  where
    moduleNodes =
      HM.foldlWithKey' (\m name n -> IM.insert n name m) mempty (st ^. moduleMap)
    st = traverse_ handleModule modules
         & runState initialState
         & run
         & fst
    initialState =
      St { _depGraph      = G.empty
         , _moduleMap     = mempty
         , _moduleCounter = 0
         }


type FD r = Member (State St) r

handleModule :: FD r => Module a -> Sem r ()
handleModule Module{..} = for_ moduleInstances $ \ModuleInstance{..} ->
  ((,) <$> getNode moduleInstanceType <*> getNode moduleName) >>= addEdge

addEdge :: FD r => (Int, Int) -> Sem r ()
addEdge (fromNode, toNode) = do
  for_ [fromNode, toNode] $ \n ->
    modify $ depGraph %~ G.insNode (n, ())
  modify $ depGraph %~ G.insEdge (fromNode, toNode, ())

getNode :: FD r => Id -> Sem r Int
getNode v = do
  res <- gets (^. moduleMap . to (HM.lookup v))
  case res of
    Nothing -> do
      n <- gets (^. moduleCounter)
      modify $
        (moduleCounter +~ 1) .
        (moduleMap %~ HM.insert v n)
      return n
    Just n -> return n
