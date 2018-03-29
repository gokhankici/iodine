{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import System.IO

import Verylog.MainCommon
import Verylog.HSFGen

main :: IO ()
main = runMain printResults

printResults          :: FilePath -> FilePath -> IO ()
printResults fin fout = do
  (qs, invs) <- hsfgen fin

  withFile fout WriteMode $ \h -> do
    let pr     = hPutStrLn h
        prLn c = pr (show c) >> pr ""
    pr "/* -*- mode: prolog -*- */"
    pr "/* vim: set ft=prolog: */\n" 
    forM_ qs   prLn
    forM_ invs prLn
  return ()

