{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO

import Verylog.MainCommon  
import Verylog.FPGen

main :: IO ()
main = runMain printResults

printResults          :: FilePath -> FilePath -> IO ()
printResults fin fout = do
  fpst <- fpgen fin

  withFile fout WriteMode $ \h -> hPutStrLn h (show fpst)
  return ()

