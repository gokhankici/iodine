{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.TransitionRelation ( next
                                            ) where

import           Control.Exception
import           Control.Monad.State.Lazy
import           Control.Lens
import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as M
import           Data.List
import           Text.Printf

import           Verylog.Transform.Utils hiding (fmt)
import           Verylog.Language.Types
import           Verylog.HSF.Types

type AsgnQueue = M.HashMap Id Int

data TRSt = TRSt { _trSt   :: St           -- state of the always block given to the function `next`
                 , _trFmt  :: VarFormat    -- store the format variable given to the function `next`
                 , _trQ    :: AsgnQueue    -- for blocking assignments only
                 , _trNBAs :: S.HashSet Id -- for sanity checking non-blocking assignments
                 }

makeLenses ''TRSt

type S = State TRSt

data Asgn = BA | NBA  

--------------------------------------------------------------------------------
next :: VarFormat -> AlwaysBlock -> HSFExpr
--------------------------------------------------------------------------------
next fmt a = Ands es
  where
    es = evalState comp (TRSt (a^.aSt) fmt M.empty S.empty)

    comp = do es1 <- nextStmt (a^.aStmt)

              ps   <- use (trSt . ports)
              q    <- use trQ
              nbas <- use trNBAs

              -- set the primed vars for the lhs of the blocking assignments
              let es2 = concat [ [ BinOp EQU
                                   (makeVar fmt{taggedVar=True, primedVar=True} v)
                                   (makeVar fmt{taggedVar=True, varId=Just n} v)
                                 , BinOp EQU
                                   (makeVar fmt{primedVar=True} v)
                                   (makeVar fmt{varId=Just n} v)
                                 ]
                               | (v,n) <- M.toList q -- blocking assignments
                               ]

              -- set the primed vars for the untouched variables
              let es3 = concat [ [ BinOp EQU
                                   (makeVar fmt{taggedVar=True, primedVar=True} v)
                                   (makeVar fmt{taggedVar=True} v) -- vt' = vt
                                 , BinOp EQU
                                   (makeVar fmt{primedVar=True} v)
                                   (makeVar fmt v) -- v' = v
                                 ]
                               | v <- ps \\ (S.toList nbas ++ M.keys q) -- not updated variables
                               ]

              return $ es1 ++ es2 ++ es3

--------------------------------------------------------------------------------
nextStmt :: Stmt -> S [HSFExpr]
--------------------------------------------------------------------------------
nextStmt (Block{..})           = sequence (nextStmt <$> blockStmts) >>= return . concat
nextStmt (BlockingAsgn{..})    = nextAsgn BA  lhs rhs
nextStmt (NonBlockingAsgn{..}) = nextAsgn NBA lhs rhs
nextStmt (IfStmt{..})          = do fmt <- use trFmt
                                    let condTrue  = BinOp GE n (Number 1)
                                        condFalse = BinOp LE n (Number 0)
                                        n = Var $ makeVarName fmt ifCond

                                    -- if condition is the value of an uninterpreted function,
                                    -- add a check that equates left & right runs for the value
                                    condUFCheck <- uf_eq ifCond

                                    -- store the current state before running either branch
                                    oldSt@(oldQ, _oldNBAs) <- getSt
                                    let qDif q = M.differenceWith
                                                 (\n' n -> if   n' == n
                                                           then Nothing
                                                           else Just n')
                                                 q oldQ

                                    -- run the then branch,
                                    thenClauses   <- nextStmt thenStmt
                                    (thQ, thNBAs) <- getSt
                                    -- and calculate what is changed
                                    let thQChanged = qDif thQ
                                    -- set back the old state
                                    setSt oldSt
                                    
                                    -- run the then branch,
                                    elseClauses <- nextStmt elseStmt
                                    (elQ, elNBAs) <- getSt
                                    -- and calculate what is changed
                                    let elQChanged = qDif elQ

                                    -- union the changes in both branches wrt the old state
                                    let qChanged  = M.unionWith (\n1 n2 -> (max n1 n2) + 1) thQChanged elQChanged

                                    -- calculate new state
                                    let newQ    = M.unionWith max oldQ qChanged
                                        newNBAs = S.union thNBAs elNBAs

                                    setSt (newQ, newNBAs)

                                    let th  = Ands $ (condTrue  : thenClauses) ++ phiNodes fmt thQ qChanged
                                        el  = Ands $ (condFalse : elseClauses) ++ phiNodes fmt elQ qChanged
                                        ite = BinOp OR th el
                                    return $ ite : condUFCheck
  where
    getSt = do q    <- use trQ
               nbas <- use trNBAs
               return (q,nbas)

    setSt :: (AsgnQueue, S.HashSet Id) -> S ()
    setSt (q,nbas) = trQ .= q >> trNBAs .= nbas

    phiNodes :: VarFormat -> AsgnQueue -> AsgnQueue -> [HSFExpr]
    phiNodes fmt q qDiff = [ BinOp EQU
                             (makeVar fmt{varId=Just n} v)
                             (makeVar fmt{varId=M.lookup v q} v)
                           | (v,n) <- M.toList qDiff
                           ]

nextStmt Skip                  = return []

--------------------------------------------------------------------------------
nextAsgn :: Asgn -> Id -> Id -> S [HSFExpr]
--------------------------------------------------------------------------------
nextAsgn a l r = do es1 <- uf_eq r
                    es2 <- case a of
                             BA  -> ba
                             NBA -> nba
                    return $ es1 ++ es2
  where
    ----------------------------------------
    ba :: S [HSFExpr]
    ----------------------------------------
    -- blocking assignment
    ba = do fmt <- use trFmt

            rhs   <- getLastVar fmt r
            t_rhs <- tagRhs

            -- use a fresh var for the assignment
            incrLastVar l
            lhs   <- getLastVar fmt            l
            t_lhs <- getLastVar (mkTagged fmt) l

            return [ BinOp EQU t_lhs t_rhs -- update the tag   of lhs
                   , BinOp EQU lhs rhs     -- update the value of lhs
                   ]

    ----------------------------------------
    nba :: S [HSFExpr]
    ----------------------------------------
    -- non-blocking assignment
    nba = do fmt <- use trFmt

             rhs   <- getLastVar fmt r
             t_rhs <- tagRhs

             -- there can be at most one non-blocking assignment for each variable,
             -- so directly set the primed variable
             let lhs   = makeVar fmt{primedVar=True}            l
                 t_lhs = makeVar (mkTagged fmt){primedVar=True} l

             -- check if there are multiple non-blocking assignments to the same variable
             uses trNBAs (S.member l)
               >>= flip when (throw . PassError $ printf "multiple non-blocking assignments to %s !" l)

             trNBAs %= S.insert l

             return [ BinOp EQU t_lhs t_rhs -- set the next tag   of lhs
                    , BinOp EQU lhs rhs     -- set the next value of lhs
                    ]

    ----------------------------------------
    tagRhs :: S HSFExpr
    ----------------------------------------
    tagRhs = do c <- isUF r
                if c
                  then ufTagRhs
                  else uses trFmt mkTagged >>= flip getLastVar r 

    ----------------------------------------
    ufTagRhs :: S HSFExpr
    ----------------------------------------
    ufTagRhs = do fmt <- uses trFmt mkTagged
                  vars <- ufAtoms fmt r
                  case vars of
                    []   -> return $ makeVar fmt l -- rhs is constant, no tag propagation needed
                    v:vs -> return $ foldr (BinOp PLUS) v vs

    mkTagged fmt = fmt{taggedVar=True}

----------------------------------------
ufAtoms :: VarFormat -> Id -> S [HSFExpr]
----------------------------------------
-- arguments of the uf formatted with fmt
ufAtoms fmt u = do atoms <- uses (trSt.ufs) (M.lookupDefault err u)
                   sequence $ getLastVar fmt <$> atoms
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

                       varsL <- ufAtoms fmtL u
                       varsR <- ufAtoms fmtR u

                       let lhs = Ands $ uncurry (BinOp EQU) <$> zip varsL varsR
                           rhs = BinOp EQU (makeVar fmtL u) (makeVar fmtR u)
                       return [makeImpl lhs rhs]
               else return []
                    
--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

isUF :: Id -> S Bool
isUF v = uses (trSt . ufs) (M.member v)

getLastVar       :: VarFormat -> Id -> S HSFExpr
getLastVar fmt v = do mi  <- uses trQ (M.lookup v)
                      return $ makeVar fmt{varId=mi} v

incrLastVar   :: Id -> S ()
incrLastVar v = trQ <~ uses trQ (M.alter updateInd v)
  where
    updateInd Nothing = Just 1
    updateInd _       = throw (PassError $ printf "multiple assignments to %s in an always block !" v)
