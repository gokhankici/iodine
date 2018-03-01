{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.TransitionRelation ( next
                                            ) where

import           Control.Exception
import           Control.Monad.State.Lazy
import           Control.Lens
import qualified Data.HashMap.Strict      as M
import           Data.List

import           Verylog.Transform.Utils hiding (fmt)
import           Verylog.Language.Types
import           Verylog.HSF.Types

type AsgnQueue = M.HashMap Id Int

data TRSt = TRSt { _trSt   :: St         -- state of the always block given to the function `next`
                 , _trFmt  :: VarFormat  -- store the format variable given to the function `next`
                 , _trAs   :: AsgnQueue  -- assignment counts
                 , _trBAs  :: AsgnQueue  -- assignment counts (blocking assignments only)
                 }

makeLenses ''TRSt

type S = State TRSt

data Asgn = BA | NBA  

--------------------------------------------------------------------------------
next :: VarFormat -> AlwaysBlock -> HSFExpr
--------------------------------------------------------------------------------
next fmt a = Ands es
  where
    es = evalState comp (TRSt (a^.aSt) fmt M.empty M.empty)

    comp = do es1 <- nextStmt (a^.aStmt)

              ps  <- use (trSt . ports)
              as  <- use trAs

              -- set the primed vars for the lhs of the assignments
              let es2 = concat [ [ BinOp EQU
                                   (makeVar fmt{taggedVar=True, primedVar=True} v)
                                   (makeVar fmt{taggedVar=True, varId=Just n} v)
                                 , BinOp EQU
                                   (makeVar fmt{primedVar=True} v)
                                   (makeVar fmt{varId=Just n} v)
                                 ]
                               | (v,n) <- M.toList as -- blocking assignments
                               ]

              -- set the primed vars for the untouched variables
              let es3 = concat [ [ BinOp EQU
                                   (makeVar fmt{taggedVar=True, primedVar=True} v)
                                   (makeVar fmt{taggedVar=True} v) -- vt' = vt
                                 , BinOp EQU
                                   (makeVar fmt{primedVar=True} v)
                                   (makeVar fmt v) -- v' = v
                                 ]
                               | v <- ps \\ M.keys as -- not updated variables
                               ]

              return $ es1 ++ es2 ++ es3

--------------------------------------------------------------------------------
nextStmt :: Stmt -> S [HSFExpr]
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
  let condTrue  = BinOp GE n (Number 1)
      condFalse = BinOp LE n (Number 0)
      n = Var $ makeVarName fmt ifCond

  -- if condition is the value of an uninterpreted function,
  -- add a check that equates left & right runs for the value
  condUFCheck <- uf_eq ifCond

  -- store the current state before running either branch
  oldSt <- getSt

  -- run the then branch,
  thenClauses   <- nextStmt thenStmt
  (thAs,thBAs)  <- getSt

  -- set back the old state
  setSt oldSt
  
  -- run the then branch,
  elseClauses   <- nextStmt elseStmt
  (elAs, elBAs) <- getSt

  -- calculate & set the new state
  let newAs  = M.unionWith max thAs  elAs
      newBAs = M.unionWith max thBAs elBAs
  setSt (newAs, newBAs)

  let thDiff = branchDif thAs elAs
      elDiff = branchDif elAs thAs
      -- thDiff  = trc "thDiff" (thAs,elAs,thDiff') thDiff'
      -- elDiff  = trc "elDiff" (elAs,thAs,elDiff') elDiff'

  let th  = Ands [ Ands (condTrue  : thenClauses)
                 , Boolean True
                 , Ands $ phiNodes fmt thAs thDiff
                 ]
      el  = Ands [ Ands (condFalse : elseClauses)
                 , Boolean True
                 , Ands $ phiNodes fmt elAs elDiff
                 ]
      ite = BinOp OR th el
  return $ ite : condUFCheck

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
    
    phiNodes :: VarFormat -> AsgnQueue -> AsgnQueue -> [HSFExpr]
    phiNodes fmt q qDiff = [ BinOp EQU
                             (makeVar fmt'{varId=Just n} v)
                             (makeVar fmt'{varId=M.lookup v q} v)
                           | (v,n) <- M.toList qDiff
                           , fmt' <- [fmt, fmt{taggedVar=True}]
                           ]

nextStmt Skip                  = return []

--------------------------------------------------------------------------------
nextAsgn :: Asgn -> Id -> Id -> S [HSFExpr]
--------------------------------------------------------------------------------
nextAsgn a l r = do es1 <- uf_eq r
                    es2 <- asgn
                    return $ es1 ++ es2
  where
    ----------------------------------------
    asgn :: S [HSFExpr]
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
    tagRhs :: S HSFExpr
    ----------------------------------------
    tagRhs = do c <- isUF r
                if c
                  then ufTagRhs
                  else uses trFmt mkTagged >>= flip getLastVarRHS r 

    ----------------------------------------
    ufTagRhs :: S HSFExpr
    ----------------------------------------
    ufTagRhs = do fmt <- uses trFmt mkTagged
                  vars <- ufAtomsRHS fmt r
                  case vars of
                    []   -> return $ makeVar fmt l -- rhs is constant, no tag propagation needed
                    v:vs -> return $ foldr (BinOp PLUS) v vs

    mkTagged fmt = fmt{taggedVar=True}

----------------------------------------
ufAtomsRHS :: VarFormat -> Id -> S [HSFExpr]
----------------------------------------
-- arguments of the uf formatted with fmt
ufAtomsRHS fmt u = do atoms <- uses (trSt.ufs) (M.lookupDefault err u)
                      sequence $ getLastVarRHS fmt <$> atoms
  where
    err = throw (PassError $ "could not find " ++ u ++ " in ufs")
             

----------------------------------------
uf_eq :: Id -> S [HSFExpr]
----------------------------------------
uf_eq u = do c <- isUF u
             if c
               then do fmt <- use trFmt
                       let fmtL = fmt{leftVar=True,  rightVar=False}
                           fmtR = fmt{leftVar=False, rightVar=True}

                       varsL <- ufAtomsRHS fmtL u
                       varsR <- ufAtomsRHS fmtR u

                       let lhs = Ands $ uncurry (BinOp EQU) <$> zip varsL varsR
                           rhs = BinOp EQU (makeVar fmtL u) (makeVar fmtR u)
                       return [makeImpl lhs rhs]
               else return []
                    
--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

isUF :: Id -> S Bool
isUF v = uses (trSt . ufs) (M.member v)

getLastVarRHS       :: VarFormat -> Id -> S HSFExpr
getLastVarRHS fmt v = do mi <- uses trBAs (M.lookup v)
                         return $ makeVar fmt{varId=mi} v

getLastVarLHS       :: VarFormat -> Id -> S HSFExpr
getLastVarLHS fmt v = do mi <- uses trAs (M.lookup v)
                         return $ makeVar fmt{varId=mi} v

incrBA :: Id -> S ()
incrBA v = do trAs  <~ uses trAs  (M.alter updateInd v)
              trBAs <~ uses trBAs (M.alter updateInd v)

incrNBA :: Id -> S ()
incrNBA v = trAs  <~ uses trAs  (M.alter updateInd v)

updateInd Nothing = Just 1
updateInd m       = (+ 1) <$> m
