{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Lens
-- import           Control.Monad
import           Data.List
-- import qualified Data.Set as DS
-- import           Data.Foldable
import           Verylog.Language.Parser (parse)
import           Verylog.Language.Types
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Verylog.Transform.DFG (stmt2Assignments)
import           Verylog.Transform.Utils
import           Verylog.FPGen
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Verylog.Transform.TransitionRelation
import           Verylog.Solver.Common
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

type Acc2 = (Bool, Id, S, S)

findFirstAssignment :: Id -> [AlwaysBlock] -> AlwaysBlock
findFirstAssignment v as = head $ filter (h . (view aStmt)) as
  where
    h :: Stmt -> Bool
    h Skip                  = False
    h (BlockingAsgn{..})    = lhs == v
    h (NonBlockingAsgn{..}) = lhs == v
    h (IfStmt{..})          = h thenStmt || h elseStmt
    h (Block{..})           = any h blockStmts  

findMissing :: Id -> HM.HashMap Id S -> S
findMissing v m = h (wl_init, HS.singleton v, HS.empty)
  where
    Just wl_init = HM.lookup v m

    h :: (S,S,S) -> S
    h (wl, dl, dk) =
      if   HS.null wl
      then dk
      else let (w:_)    = HS.toList wl
               (rs,dk') = case HM.lookup w m of
                            Nothing -> (HS.empty, HS.insert w dk)
                            Just s  -> (s, dk)
               wl'      = HS.delete w wl `HS.union` (HS.difference rs dl)
               dl'      = HS.insert w dl
           in h (wl', dl', dk')

main2 :: Options -> IO ()
main2 (Options{..}) = do
  fstr <- readFile optInputFile
  let v   = head optUnknown
      as  = pipeline' optInputFile fstr
      a   = findFirstAssignment (head optUnknown) as

      vt  = makeVarName fmt{leftVar=True,taggedVar=True} v
      (e,upds) = next fmt{leftVar=True} a

      Just (Var vt1) = lookup vt upds 

  -- printf "%s is last updated with %s\n" vt vt1

  let es  = h e
      es' = filter (\(BinOp{..}) -> let Var l = expL in "VLT" `isPrefixOf` l) es

  let m = foldl' (\m' (BinOp{..}) ->
                     let Var l = expL
                         s     = getVars expR
                     in HM.alter (\x -> case x of
                                          Nothing -> Just s
                                          Just s' -> Just $ s' `HS.union` s) l m') HM.empty es'

  sequence_ $ print <$> (HS.toList $ findMissing vt1 m)

  where
    -- returns a list of binops
    h :: Expr -> [Expr]
    h (Ands es)     = concatMap h es
    h (Ite{..})     = concatMap h [expThen, expElse]
    h b@(BinOp{..}) = [b]
    h (UFCheck{..}) = []
    h e             = error $ "unsupported expr:" ++ show e

    getVars :: Expr -> S
    getVars (Boolean _) = HS.empty
    getVars (Number _)  = HS.empty
    getVars (Var v)     = HS.singleton v
    getVars (BinOp{..}) = getVars expL `HS.union` getVars expR
    getVars e           = error $ "getVars error: unsupported " ++ show e

--------------------------------------------------------------------------------
-- MAIN FUNCTION
--------------------------------------------------------------------------------
  
main :: IO ()
main = do
  args <- parseOpts
  choice args
  where
    choice = main2
    
