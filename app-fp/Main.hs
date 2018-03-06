{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import System.IO

import Verylog.MainCommon  
import Verylog.FPGen
import Verylog.Solver.FP.Types

main :: IO ()
main = runMain printResults

printResults          :: FilePath -> FilePath -> IO ()
printResults fin fout = do
  invs <- fpgen fin

  withFile fout WriteMode $ \h -> do
    let pr     = hPutStrLn h
        prLn c = pr (show c) >> pr ""
    forM_ invs prLn
  return ()

