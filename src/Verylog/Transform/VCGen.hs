{-# LANGUAGE RecordWildCards #-}
module Verylog.Transform.VCGen ( invs
                               ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.List
import qualified Data.HashMap.Strict      as M

import           Verylog.Transform.Utils
import           Verylog.Language.Types
import           Verylog.HSF.Types

import Debug.Trace  

type R = Reader St

invs :: [AlwaysBlock] -> [HSFClause]
invs = concatMap modular_inv

modular_inv :: AlwaysBlock -> [HSFClause]  
modular_inv ab = trace (show ab) []

-- TODO
old_invs :: St -> (St, [HSFClause])
old_invs st = (st, loop st)
  where
    loop :: St -> [HSFClause]
    loop st = (single_inv st) ++ (foldr h [] (st^.irs))

    h :: IR -> [HSFClause] -> [HSFClause]
    h (ModuleInst {..}) cs  = (loop modInstSt) ++ cs
    h (Always{..}) cs       = cs

single_inv :: St -> [HSFClause]
single_inv st =
  let inv1    = runReader invInitClause st 
      invMain = runReader invMainClause st
      invProp = runReader invPropClause st
      qn      = runReader queryNaming st
  in [qn, inv1, invMain, invProp]

invInitClause :: R HSFClause
invInitClause = do args <- asks (invArgs fmt)
                   -- set the taint bits of sources to 1
                   ss <- view sources
                   let r1 = [ Ands [ BinOp EQU (lt s) (Number 1)
                                   , BinOp EQU (rt s) (Number 1)
                                   ]
                            | s <- ss
                            ]

                   -- set the taint bits of rest to 0
                   ps <- view ports
                   let vs = ps \\ ss
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

                   or4 <- asks ufEqs
                   return $ Inv argsPrimed (Ands $ or3 : or4 : Ors [or1, or2] : [] )
                                                

invMainIssueNewBit :: R HSFExpr
invMainIssueNewBit = do
  -- both executions have not finished yet
  -- done l&r old&new set to 0
  let l1 = [ BinOp EQU v (Number 0) | v <- [l,lp,r,rp] <*> [done_atom]]
  -- issue a new taint bit
  ss <- view sources
  let l2 = [ Ands [ BinOp EQU (ltp s) (Number 1)
                  , BinOp EQU (rtp s) (Number 1)
                  ]
           | s <- ss
           ]
  -- reset other taint bits
  ps <- view ports
  us <- views ufs M.keys

  -- let ps = trc "|ports|" (length _ps) _ps

  -- let us = let cnt      = (M.size _us)
  --              constCnt = foldr (\l sum -> if length l == 0 then sum + 1 else sum) 0 _us
  --              ufArgCnt = foldr (\l sum -> if any (isPrefixOf "uf_") l then sum + 1 else sum) 0 _us
  --          in trc "|ufs|, |constUF|, |hasUfArg|" (cnt,constCnt,ufArgCnt) (M.keys _us)

  let l3 = [ Ands [ BinOp EQU (ltp v) (Number 0)
                  , BinOp EQU (rtp v) (Number 0)
                  ]
           | v <- ps \\ ss
           ]
  -- all variable valuations stay the same
  let l4 = [ Ands [ BinOp EQU (lp v) (l v)
                  , BinOp EQU (rp v) (r v)
                  ]
           | v <- ps ++ us
           ]
  
  return $ Ands (l1 ++ l2 ++ l3 ++ l4)

invMainNextStep :: R HSFExpr
invMainNextStep = do
  -- both take a single step
  largs <- asks (nextArgs fmt{leftVar=True})
  rargs <- asks (nextArgs fmt{rightVar=True})

  -- conditions are the same
  cs <- getCondAtoms
  let l1 = [ BinOp EQU (l c) (r c)
           | c <- cs
           ]

  -- both read the same instructions
  ss <- view sources
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
invPropClause = do
  args <- asks (invArgs fmt)
  -- let args = trc "|invArgs|" (length _args) _args

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


ufEqs :: St -> HSFExpr
ufEqs st = Ands [ BinOp AND
                  (line fmt                 uf args)
                  (line fmt{primedVar=True} uf args)
                | (uf,args) <- st^.ufs.to M.toList
                ]
  where
    mkv f            = Var . makeVarName f
    line fmt uf args = BinOp IMPLIES (argsEq fmt args) (ufEq fmt uf)
    argsEq fmt args  = Ands (ufEq fmt <$> args)
    ufEq fmt v       = BinOp EQU (mkv fmt{leftVar=True} v) (mkv fmt{rightVar=True} v)
