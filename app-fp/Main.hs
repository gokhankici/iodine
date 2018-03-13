{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verylog.FPGen

import Language.Fixpoint.Solver
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config

import Data.List
import Data.Maybe
import System.Console.ANSI
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import Text.PrettyPrint

data Flag = VCGen
          | Debug
          | FQFile
          deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options =
  [ Option [] ["vcgen"] (NoArg VCGen) "Just vcgen, do not solve"
  , Option [] ["debug"] (NoArg Debug) "Just vcgen, do not solve"
  , Option [] ["fq"]    (NoArg FQFile) "run fixpoint on the given file"
  ]

parseOpts :: IO (Bool, FilePath, Bool)
parseOpts = do
  args <- getArgs
  return $
    case getOpt Permute options args of
      (opts,rest,[]) ->
        let b     = isJust $ find (\o -> o == VCGen || o == Debug) opts
            [fin] = rest
            fq    = isJust $ find (\o -> o == FQFile) opts
        in  (b, fin, fq)
      (_,_,errs) ->
        error (concat errs ++ usageInfo header options)
        where
          header = "Usage: vcgen-fp [OPTION...] files..."
  

main :: IO ()
main  = do
  (skipSolve, fin, runFixpoint) <- parseOpts

  finfo <- fpgen fin
  let cfg = defConfig{ eliminate = Some
                     , save      = True
                     , srcFile   = fin
                     } 
  if runFixpoint
    then solveFQ defConfig{ srcFile   = fin
                          , eliminate = Some
                          } >>= exitWith
    else if skipSolve
         then saveQuery cfg finfo >> exitSuccess
         else do res <- solve cfg finfo
                 let statStr = render . resultDoc . fmap fst
                 let stat = resStatus res
                 colorStrLn (getColor stat) (statStr stat)
                 exitWith (resultExit $ resStatus res)

colorStrLn   :: Color -> String -> IO ()
colorStrLn c = withColor c . putStrLn

withColor :: Color -> IO () -> IO ()
withColor c act
   = do setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
        act
        setSGR [ Reset]

getColor (Safe) = Green
getColor (_)    = Red
