{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verylog.Runner

import System.Console.GetOpt
import System.Environment

options :: [OptDescr Flag]
options =
  [ Option [] ["vcgen"]       (NoArg VCGen)      "Just vcgen, do not solve"
  , Option [] ["print-finfo"] (NoArg PrintFInfo) "Just vcgen, do not solve"
  , Option [] ["visualize"]   (NoArg Visualize)  "Visualize assignments"
  , Option [] ["minimize"]    (NoArg Minimize)   "print minimal failing constraint set"
  , Option [] ["no-save"]     (NoArg NoSave)     "do not save fq* files"
  , Option [] ["abduction"]   (NoArg Abduction)  "find hidden assumptions"
  ]

parseOpts :: IO Options
parseOpts = do
  args <- getArgs
  return $
    case getOpt Permute options args of
      (opts,rest,[]) ->
        let (fin, fout) = case rest of
                            [a,b] -> (a,b)
                            _     -> error $ "expected 2 arguments, got these: " ++ show rest
        in Options { optInputFile  = fin
                   , optOutputFile = fout
                   , optVCGen      = VCGen      `elem` opts
                   , optPrintFInfo = PrintFInfo `elem` opts
                   , optVisualize  = Visualize  `elem` opts
                   , optMinimize   = Minimize   `elem` opts
                   , optNoSave     = NoSave     `elem` opts
                   , optAbduction  = Abduction  `elem` opts
                   }
      (_,_,errs) ->
        error (concat errs ++ usageInfo header options)
        where
          header = "Usage: vcgen-fp [OPTION...] files..."
  
main :: IO ()
main  = parseOpts >>= run
