{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.TransitionRelation ( next
                                            ) where

import           Control.Exception
import           Control.Monad.State.Lazy
import           Control.Lens
import qualified Data.HashMap.Strict      as M
import qualified Data.HashSet             as S
import           Data.List

import           Verylog.Transform.Utils hiding (fmt)
import           Verylog.Language.Types
import           Verylog.Solver.Common

type AsgnQueue = M.HashMap Id Int

data TRSt = TRSt { _trSt    :: St           -- state of the always block given to the function `next`
                 , _trFmt   :: VarFormat    -- store the format variable given to the function `next`
                 , _trAs    :: AsgnQueue    -- assignment counts
                 , _trBAs   :: AsgnQueue    -- assignment counts (blocking assignments only)
                 , _ifConds :: S.HashSet Id -- conditions of the if expressions in the stmt hierarchy
                 , _errs    :: [PassError]
                 }

makeLenses ''TRSt

type S = State TRSt

data Asgn = BA | NBA  

type Updates = [(Id, Expr)]

--------------------------------------------------------------------------------
next :: VarFormat -> AlwaysBlock -> (Expr,Updates)
--------------------------------------------------------------------------------
next fmt a = evalState comp initSt
  where
    initSt = TRSt { _trSt    = a^.aSt
                  , _trFmt   = fmt
                  , _trAs    = M.empty
                  , _trBAs   = M.empty
                  , _ifConds = S.empty
                  , _errs    = []
                  }

    comp = do es1 <- nextStmt (a^.aStmt)

              ps  <- uses (trSt . ports) (map varName) -- . filter isRegister)
              as  <- use trAs

              -- set the primed vars for the lhs of the assignments
              let up1 = concat [ [ (,)
                                   (makeVarName fmt{taggedVar=True} v)
                                   (makeVar     fmt{taggedVar=True, varId=Just n} v)
                                 , (,)
                                   (makeVarName fmt v)
                                   (makeVar     fmt{varId=Just n} v)
                                 ]
                               | (v,n) <- M.toList as -- blocking assignments
                               , v `elem` ps
                               ]

              -- set the primed vars for the untouched variables
              let up2 = concat [ [ (,)
                                   (makeVarName fmt{taggedVar=True} v)
                                   (makeVar     fmt{taggedVar=True} v) -- vt' = vt'
                                 , (,)
                                   (makeVarName fmt v)
                                   (makeVar     fmt v) -- v' = v'
                                 ]
                               | v <- ps \\ M.keys as -- not updated variables
                               ]

              errors <- use errs
              case errors of
                []  -> return (Ands es1, up1 ++ up2)
                e:_ -> throw e

--------------------------------------------------------------------------------
nextStmt :: Stmt -> S [Expr]
--------------------------------------------------------------------------------
nextStmt (Block{..})           = sequence (nextStmt <$> blockStmts) >>= return . concat
nextStmt (BlockingAsgn{..})    = nextAsgn BA  lhs rhs
nextStmt (NonBlockingAsgn{..}) = nextAsgn NBA lhs rhs
nextStmt (IfStmt{..})          = do
  -- ---------------------------------------------------------------------------
  -- ASSUMPTION(S):
  -- 1. the types of assignments to a variable is the same in both branches
  -- 2. at most one assignment to a single variable
  -- ---------------------------------------------------------------------------
  fmt <- use trFmt

  n <- getLastVarRHS fmt ifCond
  let condTrue  = BinOp GE n (Number 1)

  -- if condition is the value of an uninterpreted function,
  -- add a check that equates left & right runs for the value
  condUFCheck <- if rightVar fmt
                 then uf_eq ifCond
                 else return []

  -- store old if conds, and calculate the new one
  oldIfConds <- use ifConds
  cvs <- ifCondVars ifCond 
  ifConds %= S.union cvs

  -- store the current state before running either branch
  oldSt <- getSt

  -- run the then branch,
  thenClauses   <- nextStmt thenStmt
  (thAs,thBAs)  <- getSt

  -- set back the old state
  setSt oldSt
  
  -- run the else branch,
  elseClauses   <- nextStmt elseStmt
  (elAs, elBAs) <- getSt

  -- calculate & set the new state
  let newAs  = M.unionWith max thAs  elAs
      newBAs = M.unionWith max thBAs elBAs

  setSt (newAs, newBAs)
  ifConds .= oldIfConds

  let thDiff = branchDif thAs elAs
      elDiff = branchDif elAs thAs

  let th  = Ands $ thenClauses ++ phiNodes fmt thAs thDiff
      el  = Ands $ elseClauses ++ phiNodes fmt elAs elDiff
      ite = Ite condTrue th el

  return $ condUFCheck ++ [ite]

  where
    -- returns the variables changed in one branch but not in other
    branchDif myQ otherQ = M.differenceWith
                           (\otherN myN -> if myN < otherN   -- if I don't have the latest number
                                           then Just otherN  -- add another equality to match that
                                           else Nothing      -- otherwise, skip
                           ) otherQ myQ

    getSt :: S (AsgnQueue, AsgnQueue)
    getSt = (,) <$> use trAs <*> use trBAs

    setSt          :: (AsgnQueue, AsgnQueue) -> S ()
    setSt (as,bas) = trAs .= as >> trBAs .= bas
    
    phiNodes :: VarFormat -> AsgnQueue -> AsgnQueue -> [Expr]
    phiNodes fmt q qDiff = [ BinOp EQU
                             (makeVar fmt'{varId=Just n} v)
                             (makeVar fmt'{varId=M.lookup v q} v)
                           | (v,n) <- M.toList qDiff
                           , fmt' <- [fmt, fmt{taggedVar=True}]
                           ]
    ifCondVars v = varDeps v >>= return . S.fromList

nextStmt Skip                  = return []

--------------------------------------------------------------------------------
nextAsgn :: Asgn -> Id -> Id -> S [Expr]
--------------------------------------------------------------------------------
nextAsgn a l r = do es1 <- uf_eq r
                    es2 <- asgn
                    return $ es1 ++ es2
  where
    ----------------------------------------
    asgn :: S [Expr]
    ----------------------------------------
    asgn = do fmt <- use trFmt

              rhs   <- getLastVarRHS fmt r
              t_rhs <- tagRhs

              -- use a fresh var for the assignment
              case a of
                BA  -> incrBA  l
                NBA -> incrNBA l

              lhs   <- getLastVarLHS fmt            l
              t_lhs <- getLastVarLHS (mkTagged fmt) l
  
              return [ BinOp EQU t_lhs t_rhs -- update the tag   of lhs
                     , BinOp EQU lhs rhs     -- update the value of lhs
                     ]

    ----------------------------------------
    tagRhs :: S Expr
    ----------------------------------------
    tagRhs = do c <- isUF r
                fmt' <- uses trFmt mkTagged
                ts1 <- if c
                       then ufTagRhs
                       else getLastVarRHS fmt' r 
                ts2 <- (uses ifConds S.toList) >>= mapM (getLastVarRHS fmt')
                case ts2 of
                  [] -> return ts1 
                  _  -> let (es1:ess) = nub $ plusVars (ts1:ts2) -- add each unique operand only once
                        in return $ foldr (BinOp PLUS) es1 ess
                
    plusVars :: [Expr] -> [Expr]
    plusVars es = concatMap f es
      where
        f e = case e of
                BinOp{..} -> concatMap f [expL, expR]
                _         -> [e]

    ----------------------------------------
    ufTagRhs :: S Expr
    ----------------------------------------
    ufTagRhs = do fmt <- uses trFmt mkTagged
                  vars <- ufAtomsRHS fmt r
                  case vars of
                    []   -> return $ Number 0 -- rhs is constant, no tag propagation needed
                    v:vs -> return $ foldr (BinOp PLUS) v vs

    mkTagged fmt = fmt{taggedVar=True}


----------------------------------------
varDeps :: Id -> S [Id]
----------------------------------------
-- recursively look into the given variables arguments
-- the returned list only contain ports
varDeps v = do c <- isUF v
               if c
                 then uses (trSt.ufs) (M.lookupDefault err v)
                 else return [v]
  where
    err = throw (PassError $ "could not find " ++ v ++ " in ufs")

----------------------------------------
ufAtomsRHS :: VarFormat -> Id -> S [Expr]
----------------------------------------
-- arguments of the uf formatted with fmt
ufAtomsRHS fmt u = do c <- isUF u
                      when (not c) $ errs %= (:) err
                      atoms <- varDeps u
                      sequence $ getLastVarRHS fmt <$> atoms
  where
    err = throw (PassError $ "could not find " ++ u ++ " in ufs")
             

----------------------------------------
uf_eq :: Id -> S [Expr]
----------------------------------------
uf_eq u = do c <- isUF u
             if c
               then do fmt <- use trFmt
                       let fmtL = fmt{leftVar=True,  rightVar=False}
                           fmtR = fmt{leftVar=False, rightVar=True}

                       varsL <- ufAtomsRHS fmtL u
                       varsR <- ufAtomsRHS fmtR u

                       let lhs = zip varsL varsR
                           rhs = (makeVar fmtL u, makeVar fmtR u)
                       return [UFCheck { ufFunc  = u
                                       , ufArgs  = lhs
                                       , ufNames = rhs
                                       }]
               else return []
                    
--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

isUF :: Id -> S Bool
isUF v = uses (trSt . ufs) (M.member v)

getLastVarRHS       :: VarFormat -> Id -> S Expr
getLastVarRHS fmt v = do mi <- uses trBAs (M.lookup v)
                         return $ makeVar fmt{varId=mi} v

getLastVarLHS       :: VarFormat -> Id -> S Expr
getLastVarLHS fmt v = do mi <- uses trAs (M.lookup v)
                         return $ makeVar fmt{varId=mi} v

incrBA :: Id -> S ()
incrBA v = do trAs  <~ uses trAs  (M.alter updateInd v)
              trBAs <~ uses trBAs (M.alter updateInd v)

incrNBA :: Id -> S ()
incrNBA v = trAs  <~ uses trAs  (M.alter updateInd v)

updateInd         :: Maybe Int -> Maybe Int
updateInd Nothing = Just 1
updateInd m       = (+ 1) <$> m
