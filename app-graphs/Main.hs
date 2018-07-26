{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Lens
-- import           Data.Foldable
import           Verylog.Language.Parser (parse)
import           Verylog.Language.Types
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Verylog.Transform.DFG (stmt2Assignments)
import           Verylog.FPGen
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
-- import           Text.Printf

--------------------------------------------------------------------------------
-- PARSING OPTIONS
--------------------------------------------------------------------------------

data Flag = ModuleGraph
          deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options =
  [ Option [] ["module-graph"] (NoArg ModuleGraph) ""
  ]

data Options = Options { optInputFile   :: FilePath
                       , optOutputFile  :: FilePath
                       , optModuleGraph :: Bool
                       , optUnknown     :: [String]
                       }

parseOpts :: IO Options
parseOpts = do
  args <- getArgs
  print args
  return $
    case getOpt Permute options args of
      (opts,rest,[]) ->
        let (fin : fout : unknownArgs) = rest
        in Options { optInputFile   = fin
                   , optOutputFile  = fout
                   , optModuleGraph = ModuleGraph `elem` opts
                   , optUnknown     = unknownArgs
                   }
      (_,_,errs) ->
        error (concat errs ++ usageInfo header options)
        where
          header = "Usage: vcgen-fp [OPTION...] files..."

--------------------------------------------------------------------------------
-- ???
--------------------------------------------------------------------------------

main1 :: Options -> IO ()
main1 args = do
  let fin  = optInputFile  args
      _fout = optOutputFile args

  fstr <- readFile fin
  let st = fst $ parse fin fstr

  let stmts = foldr (\ir ss -> case ir of
                              Always{..} -> alwaysStmt : ss
                              _          -> ss) [] (st ^. irs)

  -- rhs -> [lhs]
  let m = stmt2Assignments (Block stmts) (st ^. ufs)

  putStrLn $ show m

  return ()

type S = HS.HashSet Id
type M = HM.HashMap Id S

--------------------------------------------------------------------------------
-- QUALIFIER HELPER
--------------------------------------------------------------------------------

findFirstAssignment :: Id -> [AlwaysBlock] -> AlwaysBlock
findFirstAssignment v as = head $ filter (h . (view aStmt)) as
  where
    h :: Stmt -> Bool
    h Skip                  = False
    h (BlockingAsgn{..})    = lhs == v
    h (NonBlockingAsgn{..}) = lhs == v
    h (IfStmt{..})          = h thenStmt || h elseStmt
    h (Block{..})           = any h blockStmts  

main2 :: Options -> IO ()
main2 (Options{..}) = do
  fstr <- readFile optInputFile
  let as = pipeline' optInputFile fstr
      a  = findFirstAssignment (head optUnknown) as
  print a
  return ()

--------------------------------------------------------------------------------
-- MAIN FUNCTION
--------------------------------------------------------------------------------
  
main :: IO ()
main = do
  args <- parseOpts
  choice args
  where
    choice = main2
    
