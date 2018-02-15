module Verylog.Transform.VCGen ( invs
                               ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.List
import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as M
import           Control.Exception
import           Text.Printf

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
                   let r1 = [ Ands [ BinOp EQU (tl s) (Number 1)
                                   , BinOp EQU (tr s) (Number 1)
                                   ]
                            | s <- ss
                            ]

                   -- set the taint bits of rest to 0
                   rs <- views registers S.toList
                   ws <- views wires     S.toList
                   let vs = (rs ++ ws) \\ ss
                   let r2 = [ Ands [ BinOp EQU (tl v) (Number 0)
                                   , BinOp EQU (tr v) (Number 0)
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
  where
    l  = Var . makeVarName fmt{leftVar=True}
    r  = Var . makeVarName fmt{rightVar=True}
    tl = Var . makeVarName fmt{taggedVar=True,leftVar=True}
    tr = Var . makeVarName fmt{taggedVar=True,rightVar=True}

invMainClause :: R HSFClause
invMainClause = do args <- asks (invArgs fmt{primedVar=True})
                   return $ Inv args (Boolean True)

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
