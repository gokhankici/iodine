{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verylog.FPGen

import Language.Fixpoint.Solver
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config

import System.Console.ANSI
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import Text.PrettyPrint

data Flag = VCGen
          | PrintFInfo
          deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options =
  [ Option [] ["vcgen"] (NoArg VCGen) "Just vcgen, do not solve"
  , Option [] ["print-finfo"] (NoArg PrintFInfo) "Just vcgen, do not solve"
  ]

parseOpts :: IO (String, Bool, Bool)
parseOpts = do
  args <- getArgs
  return $
    case getOpt Permute options args of
      (opts,rest,[]) ->
        let [fin]   = rest
            skip    = VCGen `elem` opts
            prfinfo = PrintFInfo`elem` opts
        in  (fin, skip, prfinfo)
      (_,_,errs) ->
        error (concat errs ++ usageInfo header options)
        where
          header = "Usage: vcgen-fp [OPTION...] files..."
  

main :: IO ()
main  = do
  (fin, skipSolve, prFInfo) <- parseOpts

  finfo <- fpgen fin
  let cfg = defConfig{ eliminate = Some
                     , save      = True
                     , srcFile   = fin
                     } 

  case () of
    _ | skipSolve -> saveQuery cfg finfo >> exitSuccess
      | prFInfo   -> do
          fInfo <- parseFInfo [fin] :: IO (FInfo ())
          putStrLn $ show fInfo
      | otherwise -> do
          res <- solve cfg finfo
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

getColor        :: FixResult a -> Color
getColor (Safe) = Green
getColor (_)    = Red
