{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Lens
import           Data.Foldable
import           Verylog.Language.Parser (parse)
import           Verylog.Language.Types
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Verylog.Transform.DFG (stmt2Assignments)
import           Verylog.FPGen
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Text.Printf

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

main1 :: Options -> IO ()
main1 args = do
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

type S = HS.HashSet Id
type M = HM.HashMap Id S

main2 :: Options -> IO ()
main2 (Options{..}) = do
  fstr <- readFile optInputFile
  let as = pipeline' optInputFile fstr

  let stmts = Block $ (^. aStmt) <$> as
  let u = HM.unions $ (^. (aSt . ufs)) <$> as
  let m = stmt2Assignments stmts u
  traverse_ (\(k,v) -> printf "%s: %s\n" k (show $ HS.toList v)) (HM.toList m)

  -- let allSources = foldr (\a s -> s `HS.union` HS.fromList (a^.aSt^.sources)) HS.empty as

  -- res <- wl m (HS.toList allSources) HS.empty HM.empty
  -- traverse_ (\(k,v) -> printf "%s: %s\n" k (show $ HS.toList v)) (HM.toList res)

  return ()
  -- where
  --   wl :: M -> [Id] -> S -> M -> M
  --   wl _m [] _ds acc       = acc
  --   wl m (src:srcs) ds acc = let acc' = case HM.lookup src m of
  --                                         Nothing -> 
  
main :: IO ()
main = do
  args <- parseOpts
  choice args
  where
    choice = main2
    
