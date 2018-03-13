{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.VCGen ( invs
                               ) where

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.List
import qualified Data.HashSet               as S
import qualified Data.HashMap.Strict      as M
import           Text.Printf

import           Verylog.Transform.TransitionRelation
import           Verylog.Transform.Utils as U
import           Verylog.Language.Types

-- import           Verylog.Solver.HSF.Types
import           Verylog.Solver.Common

--------------------------------------------------------------------------------
invs :: [AlwaysBlock] -> [Inv]
--------------------------------------------------------------------------------
invs as = concatMap modular_inv as
          ++ non_interference_checks as
          ++ concatMap provedProperty as

--------------------------------------------------------------------------------
modular_inv :: AlwaysBlock -> [Inv]  
--------------------------------------------------------------------------------
modular_inv a = [initial_inv, tag_reset_inv, next_step_inv] <*> [a']
  where
    a' = trc (printf "\nalways block #%d:\n" (a^.aId)) a a

--------------------------------------------------------------------------------
initial_inv :: AlwaysBlock -> Inv
--------------------------------------------------------------------------------
initial_inv a = Inv { invId     = a^.aId
                    , invArgs   = args
                    , invBody   = body
                    , invParams = makeInvParams a
                    }
  where
    st    = a^.aSt
    args  = makeInvArgs fmt a
    body1 = Ands [ BinOp EQU (lvar sntz) (rvar sntz)
                 | sntz <- st^.sanitize
                 ]
    body2 = Ands [ BinOp EQU tv (Number 0)
                 | s <- st^.ports, tv <- [ltvar, rtvar] <*> [s]
                 ]
    body  = Ands [ body1, body2 ]
    

--------------------------------------------------------------------------------
tag_reset_inv :: AlwaysBlock -> Inv
--------------------------------------------------------------------------------
tag_reset_inv a = Inv { invId     = a^.aId
                      , invArgs   = args'
                      , invBody   = body
                      , invParams = makeInvParams a
                      }
  where
    st    = a^.aSt
    args  = makeInvArgs fmt a
    args' = makeInvArgs fmt{primedVar=True} a

    body = let b1 = Ands [ BinOp EQU tv (Number 1)
                         | s <- st^.sources
                         , tv <- [ltvar', rtvar'] <*> [s]
                         ]
               b2 = Ands [ BinOp EQU tv (Number 0)
                         | v <- (st^.ports) \\ (st^.sources)
                         , tv <- [ltvar', rtvar'] <*> [v]
                         ]
               b3 = Ands [ Ands [ BinOp EQU (lvar' p) (lvar p)
                                , BinOp EQU (rvar' p) (rvar p)
                                ]
                         | p <- st^.ports
                         ]
           in Ands [ b1, b2, b3
                   , Structure{ propName   = makeInvPred a
                              , propArgs   = args
                              , propParams = makeInvParams a
                              }
                   ]

--------------------------------------------------------------------------------
next_step_inv :: AlwaysBlock -> Inv 
--------------------------------------------------------------------------------
next_step_inv a = Inv { invId     = a^.aId
                      , invArgs   = args'
                      , invBody   = body
                      , invParams = makeInvParams a
                      }
  where
    args  = makeInvArgs fmt                 a
    args' = makeInvArgs fmt{primedVar=True} a
    body  = Ands [ next fmt{leftVar=True} a
                 , next fmt{rightVar=True} a
                 , Structure{ propName   = makeInvPred a
                            , propArgs   = args
                            , propParams = makeInvParams a
                            }
                 ]

--------------------------------------------------------------------------------
non_interference_checks :: [AlwaysBlock] -> [Inv]
--------------------------------------------------------------------------------
non_interference_checks as = non_int_chk as [] []
  where
    interfere (r1,w1) (r2,w2) = notDistinct r1 w2 || notDistinct r2 w1 || notDistinct w1 w2
    notDistinct s1 s2 = not . null $ S.intersection s1 s2  

    non_int_chk []     _checked cs = cs
    non_int_chk (a1:as) checked cs =
      let f (rw2, a2) cs_prev = if   interfere rw1 rw2
                                then (non_interference_inv a1 a2)
                                     : (non_interference_inv a2 a1)
                                     : cs_prev
                                else cs_prev
          cs'                 = foldr f cs checked
          rw1                 = readWriteSet a1
      in non_int_chk as ((rw1, a1):checked) cs'

type RWSet = (S.HashSet Id, S.HashSet Id)

readWriteSet :: AlwaysBlock -> RWSet
readWriteSet a = evalState (comp (a^.aStmt) >> get) (S.empty, S.empty)
  where
    us = a^.aSt^.ufs

    readVars  v = S.fromList $ M.lookupDefault [v] v us
    writeVars v = S.fromList $ if M.member v us then [] else [v]
    
    comp :: Stmt -> State RWSet ()
    comp (Block ss)            = sequence_ (comp <$> ss)
    comp (BlockingAsgn{..})    = do _1 %= S.union (readVars rhs)
                                    _2 %= S.union (writeVars lhs)
    comp (NonBlockingAsgn{..}) = do _1 %= S.union (readVars rhs)
                                    _2 %= S.union (writeVars lhs)
    comp (IfStmt{..})          = do _1 %= S.union (readVars ifCond)
                                    comp thenStmt
                                    comp elseStmt
    comp Skip                  = return ()

--------------------------------------------------------------------------------
non_interference_inv :: AlwaysBlock -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
non_interference_inv a1 a2 = Inv { invId     = a2^.aId
                                 , invArgs   = args2'
                                 , invBody   = body
                                 , invParams = makeInvParams a2
                                 }
  where
    args1  = makeInvArgs fmt a1
    args2  = makeInvArgs fmt a2
    args2' = makeInvArgs fmt{primedVar=True} a2
    body   = Ands [ next fmt{leftVar=True}  a1
                  , next fmt{rightVar=True} a1
                  , Ands [ Ands [ BinOp EQU
                                  (makeVar fmt{leftVar=True, primedVar=True} v)
                                  (makeVar fmt{leftVar=True} v)
                                , BinOp EQU
                                  (makeVar fmt{rightVar=True, primedVar=True} v)
                                  (makeVar fmt{rightVar=True} v)
                                , BinOp EQU
                                  (makeVar fmt{taggedVar=True, leftVar=True, primedVar=True} v)
                                  (makeVar fmt{taggedVar=True, leftVar=True} v)
                                , BinOp EQU
                                  (makeVar fmt{taggedVar=True, rightVar=True, primedVar=True} v)
                                  (makeVar fmt{taggedVar=True, rightVar=True} v)
                                ]
                         | v <- (a2^.aSt^.ports) \\ (a1^.aSt^.ports)
                         ]
                  , Structure{ propName   = makeInvPred a1
                             , propArgs   = args1
                             , propParams = makeInvParams a1
                             }
                  , Structure{ propName   = makeInvPred a2
                             , propArgs   = args2
                             , propParams = makeInvParams a2
                             }
                  ]

                     
provedProperty :: AlwaysBlock -> [Inv]
provedProperty a =
  [ Prop { propR = BinOp GE (rtvar s) (Number 1)
         , propL = Ands [ Structure{ propName   = makeInvPred a
                                   , propArgs   = args
                                   , propParams = makeInvParams a
                                   }
                        , BinOp GE (ltvar s) (Number 1)
                        ]
         }
  | s <- a^.aSt^.sinks
  ]
  where
    args = makeInvArgs fmt a

-------------------------------------------------------------------------------- 
-- Helper functions
-------------------------------------------------------------------------------- 
lvar  = makeVar fmt{leftVar=True}
rvar  = makeVar fmt{rightVar=True}
ltvar = makeVar fmt{taggedVar=True, leftVar=True}
rtvar = makeVar fmt{taggedVar=True, rightVar=True}

lvar'  = makeVar fmt{primedVar=True, leftVar=True}
rvar'  = makeVar fmt{primedVar=True, rightVar=True}
ltvar' = makeVar fmt{primedVar=True, taggedVar=True, leftVar=True}
rtvar' = makeVar fmt{primedVar=True, taggedVar=True, rightVar=True}
