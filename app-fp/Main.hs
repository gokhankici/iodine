{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verylog.MainCommon  
import Verylog.FPGen

import Language.Fixpoint.Types.Config
import Language.Fixpoint.Types.Constraints

main :: IO ()
main = runMain printResults

printResults  :: FilePath -> FilePath -> IO ()
printResults fin fout = do
  finfo <- fpgen fin
  writeFInfo defConfig finfo fout

