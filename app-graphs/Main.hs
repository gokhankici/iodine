{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.List
import qualified Data.Set as DS
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
import           Text.Printf

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

-- goUp :: HM.HashMap Id [Id] -> Stmt -> Acc2 -> Acc2
-- goUp _  Skip acc              = acc
-- goUp us (IfStmt{..}) acc      = foldr (goUp us) acc [thenStmt, elseStmt]
-- goUp us (Block{..})  acc      = foldr (goUp us) acc blockStmts
-- goUp us s acc@(start,v,wl,dl) = if not start && l /= v then acc else acc'
--   where
--     acc' = (True, v, wl', dl')
--     wl'  = HS.delete l wl
--     dl'  = if   HS.member l dl
--            then dl
--            else 

--     l    = lhs s
--     rs   = case HM.lookup (rhs s) us of
--              Nothing -> HS.singleton (rhs s)
--              Just vs -> HS.fromList vs

main2 :: Options -> IO ()
main2 (Options{..}) = do
  fstr <- readFile optInputFile
  let v   = head optUnknown
      as  = pipeline' optInputFile fstr
      a   = findFirstAssignment (head optUnknown) as

      vt  = makeVarName fmt{leftVar=True,taggedVar=True} v
      (e,upds) = next fmt{leftVar=True} a

      Just (Var vt1) = lookup vt upds 

  printf "%s is last updated with %s" vt vt1

  let es  = h e
      es' = filter (\(BinOp{..}) -> let Var l = expL in "VLT" `isPrefixOf` l) es

  when ([EQU] /= (DS.toList . DS.fromList $ bOp <$> es')) $
    error "OH NO!"
  
  sequence_ $ print <$> es'
  print "done"

  where
    h :: Expr -> [Expr]
    h (Ands es)     = concatMap h es
    h (Ite{..})     = concatMap h [expThen, expElse]
    h b@(BinOp{..}) = [b]
    h (UFCheck{..}) = []
    h e             = error $ "unsupported expr:" ++ show e

--------------------------------------------------------------------------------
-- MAIN FUNCTION
--------------------------------------------------------------------------------
  
main :: IO ()
main = do
  args <- parseOpts
  choice args
  where
    choice = main2
    
