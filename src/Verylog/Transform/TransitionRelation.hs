{-# LANGUAGE RecordWildCards #-}
module Verylog.Transform.TransitionRelation ( next
                                            ) where

import           Control.Exception
import           Control.Monad.Reader
import           Control.Lens
import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as M

import           Verylog.Transform.Utils
import           Verylog.Language.Types
import           Verylog.HSF.Types

type R = Reader St

data Asgn = CA | BA | NBA  

next :: St -> (St, [HSFClause])
next st = (st, (: []) $ nextClause st $ concat $ readIRs st nextIR)

nextClause       :: St -> [HSFExpr] -> HSFClause
nextClause st es = Next { hsfArgs = nextArgs st
                        , hsfBody = Ands (es ++ rest)
                        }
  where
    rest = ( nextSink <$> st^.sinks.to S.toList )
           ++ [ BinOp EQU (Var $ makeVarName fmt done_atom) (Number 0) ]

nextArgs    :: St -> [Id]
nextArgs st = let rs = st^.registers.to S.toList 
                  ws = st^.wires.to     S.toList
                  us = st^.ufs.to       M.keys
                  vs = rs ++ ws
                  os = us ++ [done_atom]
              in    (makeVarName fmt                                <$> vs)
                 ++ (makeVarName fmt{taggedVar=True}                <$> vs)
                 ++ (makeVarName fmt                                <$> os)
                 ++ (makeVarName fmt{primedVar=True}                <$> vs)
                 ++ (makeVarName fmt{taggedVar=True,primedVar=True} <$> vs)
                 ++ (makeVarName fmt{primedVar=True}                <$> os)

nextIR :: IR -> R [HSFExpr]
nextIR (Always _ s)   = nextStmt s
nextIR (ContAsgn l r) = nextAsgn CA l r
nextIR e              = throw (PassError $ "nextIR for " ++ pprint e ++ " is missing") 

nextStmt :: Stmt -> R [HSFExpr]
nextStmt (Block{..})           = concat <$> mapM nextStmt blockStmts
nextStmt (BlockingAsgn{..})    = nextAsgn BA  lhs rhs
nextStmt (NonBlockingAsgn{..}) = nextAsgn NBA lhs rhs
nextStmt (IfStmt{..})          = do let condTrue  = BinOp GE n (Number 1)
                                        condFalse = BinOp LE n (Number 0)
                                    thenClauses <- nextStmt thenStmt
                                    elseClauses <- nextStmt elseStmt
                                    let th = Ands (condTrue  : thenClauses)
                                        el = Ands (condFalse : elseClauses)
                                    return $ [BinOp OR th el]
  where
    n = Var $ makeVarName fmt ifCond

nextStmt Skip                  = return []

nextAsgn :: Asgn -> Id -> Id -> R [HSFExpr]
nextAsgn a l r = do cond <- isUF r
                    if cond then asgnUF else return [asgn]

  where
    vl     = Var $ makeVarName fmt l
    vl1    = Var $ makeVarName fmt{primedVar=True} l
    vlt    = Var $ makeVarName fmt{taggedVar=True} l
    vlt1   = Var $ makeVarName fmt{primedVar=True,taggedVar=True} l
    vr     = Var $ makeVarName fmt r
    vr1    = Var $ makeVarName fmt{primedVar=True} r
    vrt    = Var $ makeVarName fmt{taggedVar=True} r
    vrt1   = Var $ makeVarName fmt{primedVar=True,taggedVar=True} r   

    asgnUF = do atoms <- views ufs (M.lookupDefault err r)
                case (Var . makeVarName fmt{taggedVar=True}) <$> atoms of
                  []   -> return [Boolean True]
                  v:vs -> let rhs = foldr (BinOp PLUS) v vs
                          in return [BinOp EQU vlt1 rhs]
    err    = throw (PassError $ "could not find " ++ r ++ " in ufs")

    asgn   = case a of
               CA -> Ands [ BinOp EQU vl vr
                          , BinOp EQU vl1 vr1
                          , BinOp EQU vlt vrt
                          , BinOp EQU vlt1 vrt1
                          ]
               _  -> Ands [ BinOp EQU vlt1 vrt
                          , BinOp EQU vl1 vr
                          ]
                    
nextSink :: Id -> HSFExpr
nextSink s =
  let st1 = Var $ makeVarName fmt{taggedVar=True,primedVar=True} s
      vd  = Var $ makeVarName fmt done_atom
      vd1 = Var $ makeVarName fmt{primedVar=True} done_atom
  in  Ite (BinOp GE st1 (Number 1)) (BinOp EQU vd1 (Number 1)) (BinOp EQU vd1 vd)
