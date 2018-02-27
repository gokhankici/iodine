{-# LANGUAGE RecordWildCards #-}
module Verylog.Transform.TransitionRelation ( next
                                            ) where

import           Control.Exception
import           Control.Monad.Reader
import           Control.Lens
import qualified Data.HashMap.Strict      as M

import           Verylog.Transform.Utils
import           Verylog.Language.Types
import           Verylog.HSF.Types

type R = Reader St

data Asgn = BA | NBA  

next :: St -> (St, [HSFClause])
next st = (st, (: []) $ nextClause st $ concat $ readIRs st nextIR)

nextClause       :: St -> [HSFExpr] -> HSFClause
nextClause st es =
  let doneIfSinkTainted = Ands $ nextSink <$> st^.sinks 
      notDone           = BinOp EQU (Var $ makeVarName fmt done_atom) (Number 0) 
      transitions       = doneIfSinkTainted : notDone : es
  in Next { hsfArgs = nextArgs fmt st
          , hsfBody = Ands transitions
          }

nextIR :: IR -> R [HSFExpr]
nextIR (Always _ s)     = nextStmt s
nextIR (ModuleInst{..}) = return . concat $ readIRs modInstSt nextIR

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
nextAsgn _ l r = do cond <- isUF r
                    if cond then asgnUF else return [asgn]

  where
    vl1 = v1 l; vlt1 = vt1 l;
    vr = v r; vrt = vt r;
    asgnUF = do atoms <- views ufs (M.lookupDefault err r)
                case (Var . makeVarName fmt{taggedVar=True}) <$> atoms of
                  []   -> return [Boolean True]
                  v:vs -> let rhs = foldr (BinOp PLUS) v vs
                          in return [BinOp EQU vlt1 rhs]
    err    = throw (PassError $ "could not find " ++ r ++ " in ufs")

    asgn   = Ands [ BinOp EQU vlt1 vrt
                  , BinOp EQU vl1 vr
                  ]
                    
nextSink :: Id -> HSFExpr
nextSink s =
  let st1 = Var $ makeVarName fmt{taggedVar=True,primedVar=True} s
      vd  = Var $ makeVarName fmt done_atom
      vd1 = Var $ makeVarName fmt{primedVar=True} done_atom
  in  Ite (BinOp GE st1 (Number 1)) (BinOp EQU vd1 (Number 1)) (BinOp EQU vd1 vd)


v   = Var . makeVarName fmt
v1  = Var . makeVarName fmt{primedVar=True}
vt  = Var . makeVarName fmt{taggedVar=True}
vt1 = Var . makeVarName fmt{primedVar=True,taggedVar=True}
