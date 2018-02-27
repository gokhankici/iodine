{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.TransitionRelation ( next
                                            ) where

import           Control.Exception
import           Control.Monad.Reader
import           Control.Lens
import qualified Data.HashMap.Strict      as M

import           Verylog.Transform.Utils
import           Verylog.Language.Types
import           Verylog.HSF.Types

data TRSt = TRSt { _trSt  :: St
                 , _trFmt :: VarFormat
                 }

makeLenses ''TRSt

type R = Reader TRSt

data Asgn = BA | NBA  

next :: VarFormat -> AlwaysBlock -> HSFExpr
next fmt a = Ands es
  where
    es = runReader (nextStmt (a^.aStmt)) (TRSt (a^.aSt) fmt)

nextStmt :: Stmt -> R [HSFExpr]
nextStmt (Block{..})           = concat <$> mapM nextStmt blockStmts
nextStmt (BlockingAsgn{..})    = nextAsgn BA  lhs rhs
nextStmt (NonBlockingAsgn{..}) = nextAsgn NBA lhs rhs
nextStmt (IfStmt{..})          = do fmt <- view trFmt
                                    let condTrue  = BinOp GE n (Number 1)
                                        condFalse = BinOp LE n (Number 0)
                                        n = Var $ makeVarName fmt ifCond
                                    thenClauses <- nextStmt thenStmt
                                    elseClauses <- nextStmt elseStmt
                                    let th = Ands (condTrue  : thenClauses)
                                        el = Ands (condFalse : elseClauses)
                                    return $ [BinOp OR th el]
nextStmt Skip                  = return []

nextAsgn :: Asgn -> Id -> Id -> R [HSFExpr]
nextAsgn _ l r = do cond <- isUF r
                    if cond then asgnUF else asgn

  where
    asgnUF = do atoms <- views trSt (M.lookupDefault err r . view ufs)
                fmt <- view trFmt
                let vlt1 = vt1 fmt l
                case (Var . makeVarName fmt{taggedVar=True}) <$> atoms of
                  []   -> return [Boolean True]
                  v:vs -> let rhs = foldr (BinOp PLUS) v vs
                          in return [BinOp EQU vlt1 rhs]
    err    = throw (PassError $ "could not find " ++ r ++ " in ufs")

    asgn   = do fmt <- view trFmt
                let vl1  = v1 fmt l
                    vlt1 = vt1 fmt l
                    vr   = v fmt r
                    vrt  = vt fmt r
                return [ Ands [ BinOp EQU vlt1 vrt
                              , BinOp EQU vl1  vr
                              ]
                       ]
                    
v   fmt = makeVar fmt
v1  fmt = makeVar fmt{primedVar=True}
vt  fmt = makeVar fmt{taggedVar=True}
vt1 fmt = makeVar fmt{primedVar=True,taggedVar=True}


isUF   :: Id -> R Bool
isUF v = views trSt (M.member v . view ufs)
