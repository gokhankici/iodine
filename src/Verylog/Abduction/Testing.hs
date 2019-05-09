{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This is a module used for debugging/testing stuff
-- | Not needed by the rest of the modules

module Verylog.Abduction.Testing where

-- import Verylog.Abduction.Graph
-- import Verylog.Abduction.Runner
-- import Verylog.Abduction.Types hiding (t)
-- import Verylog.Language.Types

-- import           Control.Monad
-- import           Data.Maybe           (fromJust)
-- import           Text.Printf
-- import qualified Data.Aeson           as J
-- import qualified Data.ByteString.Lazy as B
-- import qualified Data.Graph.Inductive as Gr
-- import qualified Data.IntMap.Strict   as IM
-- import qualified Data.Text            as T

-- var :: Id
-- var = "IF_PC_d_out"

-- t :: IO ()
-- t = do
--   g <- parseGraph
--   let snks = [findNodeId g var]
--       ms   = cplexToMark g snks 
--       ms'  = findNodeName g <$> ms
--   forM_ ms' (putStrLn . T.unpack)

-- t2 :: IO ()
-- t2 = do
--   g <- parseGraph
--   let snks = [findNodeId g var]
--   runner3' g snks

-- findNodeName :: G -> Int -> Id
-- findNodeName g n =
--   case [name | (n', name) <- Gr.labNodes g, n' == n] of
--     [name] -> name
--     _      -> error $ "could not find name of the node " ++ (show n)

-- findNodeId :: G -> Id -> Int
-- findNodeId g name =
--   case [n | (n, name') <- Gr.labNodes g, name' == name] of
--     [n] -> n
--     _   -> error $ printf "could not find name of the node %s" name

-- printEdges :: G -> IO ()
-- printEdges g = forM_ es (printEdge m)
--   where
--     m  = IM.fromList $ Gr.labNodes g
--     es = Gr.labEdges g

-- parseGraph :: IO G
-- parseGraph = (uncurry Gr.mkGraph) . fromJust . J.decode <$>
--              B.readFile jsonPath
--   where
--     jsonPath = "scripts/linprog/graph.json"

-- printEdge :: IM.IntMap Id -> (Int, Int, EdgeData) -> IO ()
-- printEdge m (u, v, typ) = printf "%30s %s %s\n" u' a v'
--   where
--     u' = m IM.! u
--     v' = m IM.! v
--     a  = case typ of
--            Direct   -> "====>" :: String
--            Implicit -> "....>"
