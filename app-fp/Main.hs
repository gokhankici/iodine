{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verylog.FPGen

import Language.Fixpoint.Solver
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config

import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import Text.PrettyPrint

import Debug.Trace

data Flag = VCGen
          | Debug
          deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options =
  [ Option [] ["vcgen"] (NoArg VCGen) "Just vcgen, do not solve"
  , Option [] ["debug"] (NoArg Debug) "Just vcgen, do not solve"
  ]

main :: IO ()
main  = do
  args <- getArgs
  let (skipSolve,fin) =
        case getOpt Permute options args of
          (opts,rest,[]) ->
            let b     = isJust $ find (\o -> o == VCGen || o == Debug) $ trace (show opts) opts
                [fin] = rest
            in  (b, fin)
          (_,_,errs) ->
            error (concat errs ++ usageInfo header options)
            where
              header = "Usage: vcgen-fp [OPTION...] files..."

  finfo <- fpgen fin
  let cfg = defConfig{ eliminate = Some
                     , save      = True
                     , srcFile   = fin
                     } 
  if skipSolve
    then saveQuery cfg finfo >> exitSuccess
    else do res <- solve cfg finfo
            let statStr = render . resultDoc . fmap fst
            putStrLn $ statStr $ resStatus res
            exitWith (resultExit $ resStatus res)
