{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import System.IO

import Verylog.MainCommon  
import Verylog.SMTGen
import Verylog.Solver.SMT.Types

main :: IO ()
main = runMain printResults

printResults          :: FilePath -> FilePath -> IO ()
printResults fin fout = do
  gen <- smtgen fin

  withFile fout WriteMode $ \h -> do
    let pr     = hPutStrLn h
        prLn c = pr (show c) >> pr ""
    forM_ gen prLn
  return ()

