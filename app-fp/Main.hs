{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verylog.MainCommon  
import Verylog.FPGen

import Language.Fixpoint.Solver
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config

import System.Exit
import Text.PrettyPrint

main :: IO ()
main = runMain printResults

printResults  :: FilePath -> FilePath -> IO ()
printResults fin _fout = do
  finfo <- fpgen fin
  let cfg = defConfig{ eliminate = Some
                     , save      = True
                     } 

  res <- solve cfg finfo
  let statStr = render . resultDoc . fmap fst
  putStrLn $ statStr $ resStatus res

  exitWith (resultExit $ resStatus res)
