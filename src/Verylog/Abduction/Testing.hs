{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

-- | This is a module used for debugging/testing stuff
-- | Not needed by the rest of the modules

module Verylog.Abduction.Testing where

import Verylog.Abduction.Graph

import Control.Monad
import System.FilePath.Posix
import System.Directory
import Text.Printf

import qualified Data.IntMap.Strict       as IM
import qualified Data.Aeson               as J
import qualified Data.Graph.Inductive     as Gr
import qualified Data.ByteString.Lazy     as B

parseGraph :: IO G
parseGraph = do file <- (</> relPath) <$> getCurrentDirectory
                (nodes, edges) <- unpack . J.decode <$> B.readFile file
                return $ Gr.mkGraph nodes edges
  where
    unpack r = case r of
                 Just r' -> r'
                 Nothing -> error "parse failed"
    relPath = "scripts" </> "linprog" </> "graph.json"

t :: IO ()
t = do
  g <- parseGraph
  let m  = IM.fromList $ Gr.labNodes g
  let es = Gr.labEdges g
  forM_ es $ \(u,v,typ) -> do
    let u' = m IM.! u
        v' = m IM.! v
    case typ of
      Direct   -> printf "%20s ====> %-20s\n" u' v'
      Implicit -> printf "%20s - - > %-20s\n" u' v'
