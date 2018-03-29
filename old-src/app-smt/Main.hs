{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import System.IO

import Verylog.MainCommon  
import Verylog.SMTGen

main :: IO ()
main = runMain printResults

printResults          :: FilePath -> FilePath -> IO ()
printResults fin fout = do
  (gen, fs, ufcs) <- smtgen fin

  withFile fout WriteMode $ \h -> do
    let pr     = hPutStrLn h
        prLn c = pr (show c) >> pr ""
    hPutStrLn h "(set-logic HORN)\n"
    forM_ fs   prLn
    forM_ ufcs prLn
    forM_ gen  prLn
    hPutStrLn h "(check-sat)"
    hPutStrLn h "(get-model)"
  return ()

