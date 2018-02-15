{-# LANGUAGE RecordWildCards #-}
module Verylog.Transform.VCGen ( invs
                               ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.List
import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as M
-- import           Control.Exception

import           Verylog.Transform.Utils
import           Verylog.Language.Types
import           Verylog.HSF.Types

type R = Reader St

invs :: St -> (St, [HSFClause])
invs st = let inv1    = runReader invInitClause st 
              invMain = runReader invMainClause st
              invProp = runReader invPropClause st
              qn      = runReader queryNaming st
          in (st, [qn, inv1, invMain, invProp])

invInitClause :: R HSFClause
invInitClause = do args <- asks (invArgs fmt)
                   -- set the taint bits of sources to 1
                   ss <- views sources S.toList
                   let r1 = [ Ands [ BinOp EQU (lt s) (Number 1)
                                   , BinOp EQU (rt s) (Number 1)
                                   ]
                            | s <- ss
                            ]

                   -- set the taint bits of rest to 0
                   rs <- views registers S.toList
                   ws <- views wires     S.toList
                   let vs = (rs ++ ws) \\ ss
                   let r2 = [ Ands [ BinOp EQU (lt v) (Number 0)
                                   , BinOp EQU (rt v) (Number 0)
                                   ]
                            | v <- vs
                            ]

                   -- both runs are not done yet
                   let r3 = [ BinOp EQU (l done_atom) (Number 0)
                            , BinOp EQU (r done_atom) (Number 0)
                            ]

                   -- initial values of the source regs are the same
                   let r4 = [ BinOp EQU (l s) (r s)
                            | s <- ss
                            ]
                   return $ Inv args $ Ands (r1 ++ r2 ++ r3 ++ r4)

l   = Var . makeVarName fmt{leftVar=True}
r   = Var . makeVarName fmt{rightVar=True}
lp  = Var . makeVarName fmt{primedVar=True,leftVar=True}
rp  = Var . makeVarName fmt{primedVar=True,rightVar=True}
lt  = Var . makeVarName fmt{taggedVar=True,leftVar=True}
rt  = Var . makeVarName fmt{taggedVar=True,rightVar=True}
ltp = Var . makeVarName fmt{primedVar=True,taggedVar=True,leftVar=True}
rtp = Var . makeVarName fmt{primedVar=True,taggedVar=True,rightVar=True}

invMainClause :: R HSFClause
invMainClause = do args       <- asks (invArgs fmt)
                   argsPrimed <- asks (invArgs fmt{primedVar=True})

                   or1 <- invMainIssueNewBit
                   or2 <- invMainNextStep

                   let or3 = Structure invPred args 
                   return $ Inv argsPrimed (Ands [Ors [or1, or2], or3])
                                                

invMainIssueNewBit :: R HSFExpr
invMainIssueNewBit = do
  -- both executions have not finished yet
  -- done l&r old&new set to 0
  let l1 = [ BinOp EQU v (Number 0) | v <- [l,lp,r,rp] <*> [done_atom]]
  -- issue a new taint bit
  ss <- views sources S.toList
  let l2 = [ Ands [ BinOp EQU (ltp s) (Number 1)
                  , BinOp EQU (rtp s) (Number 1)
                  ]
           | s <- ss
           ]
  -- reset other taint bits
  rs <- views registers S.toList
  ws <- views wires     S.toList
  us <- views ufs       M.keys
  let l3 = [ Ands [ BinOp EQU (ltp v) (Number 0)
                  , BinOp EQU (rtp v) (Number 0)
                  ]
           | v <- (rs ++ ws) \\ ss
           ]
  -- all variable valuations stay the same
  let l4 = [ Ands [ BinOp EQU (lp v) (l v)
                  , BinOp EQU (rp v) (r v)
                  ]
           | v <- rs ++ ws ++ us
           ]
  
  return $ Ands (l1 ++ l2 ++ l3 ++ l4)

invMainNextStep :: R HSFExpr
invMainNextStep = do
  -- both take a single step
  largs <- asks (invArgs fmt{leftVar=True})
  rargs <- asks (invArgs fmt{rightVar=True})

  -- conditions are the same
  cs <- getCondAtoms
  let l1 = [ BinOp EQU (l c) (r c)
           | c <- cs
           ]

  -- both read the same instructions
  ss <- views sources S.toList
  let l2 = [ Ands [ BinOp EQU (lp s) (rp s)
                  , BinOp EQU (ltp s) (Number 0)
                  , BinOp EQU (rtp s) (Number 0)
                  ]
           | s <- ss
           ]

  return $ Ands $ [ Structure nextPred largs
                  , Structure nextPred rargs
                  ] ++ l1 ++ l2

getCondAtoms :: R [Id]
getCondAtoms = views irs (concat . map irHelper)
  where
    irHelper (Always _ s) = stmtHelper s
    irHelper _            = []

    stmtHelper (IfStmt{..}) = [ifCond] ++ stmtHelper thenStmt ++ stmtHelper elseStmt
    stmtHelper (Block ss)   = concatMap stmtHelper ss
    stmtHelper _            = []
  

invPropClause :: R HSFClause
invPropClause = do args <- asks (invArgs fmt)
                   let vld = Var $ makeVarName fmt{leftVar=True} done_atom
                       vrd = Var $ makeVarName fmt{rightVar=True} done_atom
                       r   = Prop
                             (BinOp EQU vrd (Number 1))
                             (Ands [ Structure "inv" args
                                   , BinOp EQU vld (Number 1)
                                   ])
                   return r
                     
queryNaming :: R HSFClause
queryNaming = asks (invArgs fmt{atomVar=True}) >>= (return . QueryNaming)
