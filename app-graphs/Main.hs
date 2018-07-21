{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Lens
import           Verylog.Language.Parser (parse)
import           Verylog.Language.Types
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Verylog.Transform.DFG (stmt2Assignments)

data Flag = ModuleGraph
          deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options =
  [ Option [] ["module-graph"] (NoArg ModuleGraph) ""
  ]

data Options = Options { optInputFile   :: FilePath
                       , optOutputFile  :: FilePath
                       , optModuleGraph :: Bool
                       }

parseOpts :: IO Options
parseOpts = do
  args <- getArgs
  return $
    case getOpt Permute options args of
      (opts,rest,[]) ->
        let [fin, fout] = rest
        in Options { optInputFile  = fin
                   , optOutputFile = fout
                   , optModuleGraph = ModuleGraph `elem` opts
                   }
      (_,_,errs) ->
        error (concat errs ++ usageInfo header options)
        where
          header = "Usage: vcgen-fp [OPTION...] files..."

main :: IO ()
main = do
  args <- parseOpts

  let fin  = optInputFile  args
      _fout = optOutputFile args

  fstr <- readFile fin
  let st = parse fin fstr

  let stmts = foldr (\ir ss -> case ir of
                              Always{..} -> alwaysStmt : ss
                              _          -> ss) [] (st ^. irs)

  -- rhs -> [lhs]
  let m = stmt2Assignments (Block stmts) (st ^. ufs)

  putStrLn $ show m

  return ()
