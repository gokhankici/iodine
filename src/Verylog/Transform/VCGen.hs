{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.VCGen ( invs
                               ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.List
import qualified Data.HashSet               as S
import qualified Data.HashMap.Strict      as M

import           Verylog.Transform.TransitionRelation
import           Verylog.Transform.Utils as U
import           Verylog.Language.Types

import           Verylog.Solver.Common
import           Text.Printf

data PropertyOptions = PropertyOptions { checkTagEq :: Bool
                                       , checkValEq :: Bool
                                       }

defaultPropertyOptions :: PropertyOptions
defaultPropertyOptions = PropertyOptions { checkTagEq = True
                                         , checkValEq = False
                                         }

--------------------------------------------------------------------------------
invs :: [AlwaysBlock] -> [Inv]
--------------------------------------------------------------------------------
invs as = concatMap modular_inv as
          ++ non_interference_checks as
          ++ concatMap (provedProperty defaultPropertyOptions) as

--------------------------------------------------------------------------------
modular_inv :: AlwaysBlock -> [Inv]  
--------------------------------------------------------------------------------
modular_inv a = [initial_inv, tag_reset_inv, next_step_inv] <*> [a']
  where
    a' = trc (printf "\nalways block #%d:\n" (a^.aId)) a a

--------------------------------------------------------------------------------
initial_inv :: AlwaysBlock -> Inv
--------------------------------------------------------------------------------
initial_inv a = Horn { hBody = Boolean True
                     , hHead = Ands [ KV { kvId   = a ^. aId
                                         , kvSubs = sub1 ++ sub2
                                         }
                                    ]
                     , hId   = HornId (a ^. aId) InvInit
                     }
  where
    st   = a ^. aSt
    sub1 = [ (n_lvar' sntz, rvar' sntz)
           | sntz <- S.toList . S.fromList $ st ^. sanitize ++ st ^. sources ++ st ^. sinks
           ]
    sub2 = [ (tv, Number 0)
           | s <- st^.ports, tv <- [n_ltvar', n_rtvar'] <*> [s]
           ]

--------------------------------------------------------------------------------
tag_reset_inv :: AlwaysBlock -> Inv
--------------------------------------------------------------------------------
tag_reset_inv a = Horn { hBody = prevKV a
                       , hHead = KV { kvId   = a^.aId
                                    , kvSubs = hsubs
                                    }
                       , hId   = HornId (a ^. aId) InvReTag
                       }
  where
    st    = a^.aSt

    hsubs  = hsubs1 ++ hsubs2 ++ hsubs3
    hsubs1 = concat [ [ (n_lvar' p, lvar p)
                      , (n_rvar' p, rvar p)
                      ]
                    | p <- st^.ports
                    ]
    hsubs2 = [ (tv, Number 1)
             | s <- st^.sources
             , tv <- [n_ltvar', n_rtvar'] <*> [s]
             ]
    hsubs3 = [ (tv, Number 0)
             | v <- (st^.ports) \\ (st^.sources)
             , tv <- [n_ltvar', n_rtvar'] <*> [v]
             ]

--------------------------------------------------------------------------------
next_step_inv :: AlwaysBlock -> Inv 
--------------------------------------------------------------------------------
next_step_inv a = Horn { hBody = body
                       , hHead = KV { kvId   = a^.aId
                                    , kvSubs = ul ++ ur
                                    }
                       , hId   = HornId (a ^. aId) InvNext
                       }
  where
    (nl,ul)  = next fmt{leftVar=True}  a
    (nr,ur)  = next fmt{rightVar=True} a
    body     = Ands [ prevKV a
                    , sanGlobs a, taintEqs a
                    , nl, nr
                    ]

-- sanitize globs are always the same
sanGlobs   :: AlwaysBlock -> Expr
sanGlobs a = Ands $ concat [ [ BinOp EQU
                              (makeVar fmt{leftVar=True} v)
                              (makeVar fmt{rightVar=True} v)
                              , BinOp EQU
                              (makeVar fmt{taggedVar=True, leftVar=True} v)
                              (makeVar fmt{taggedVar=True, rightVar=True} v)
                             ]
                           | v <- (a ^. aSt ^. ports) `intersect` (a ^. aSt ^. sanitizeGlob)
                           ]

taintEqs   :: AlwaysBlock -> Expr
taintEqs a = Ands [ BinOp EQU
                    (makeVar fmt{taggedVar=True, leftVar=True} v)
                    (makeVar fmt{taggedVar=True, rightVar=True} v)
                  | v <- a ^. aSt ^. taintEq
                  ]
--------------------------------------------------------------------------------
non_interference_checks :: [AlwaysBlock] -> [Inv]
--------------------------------------------------------------------------------
non_interference_checks as = non_int_chk as [] []
  where
    interfere (r1,w1) (r2,w2) = notDistinct r1 w2 || notDistinct r2 w1 || notDistinct w1 w2
    notDistinct s1 s2 = not . null $ S.intersection s1 s2  

    non_int_chk []     _checked cs = cs
    non_int_chk (a1:a1s) checked cs =
      let f (rw2, a2) cs_prev = if   interfere rw1 rw2
                                then (non_interference_inv a1 a2)
                                     : (non_interference_inv a2 a1)
                                     : cs_prev
                                else cs_prev
          cs'                 = foldr f cs checked
          rw1                 = readWriteSet a1
      in non_int_chk a1s ((rw1, a1):checked) cs'

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
-- when a1 takes a step, a2 still holds
non_interference_inv a1 a2 = Horn { hBody = body
                                  , hHead = KV { kvId   = a2 ^. aId
                                               , kvSubs = updates2_1 ++ updates2_2
                                               }
                                  , hId   = HornId (a2 ^. aId) (InvInter (a1 ^. aId))
                                  }
  where
    (nl1,ul1) = next fmt{leftVar=True}  a1
    (nr1,ur1) = next fmt{rightVar=True} a1
    updates1  = ul1 ++ ur1
    lukap v   = case lookup v updates1 of
                  Nothing -> throw $ PassError "cannot find v in updates1"
                  Just e  -> (v,e)
    updates2_1  = concat [ [ (n_lvar' v,  lvar v)  -- l' = l
                           , (n_rvar' v,  rvar v)  -- r' = r
                           , (n_ltvar' v, ltvar v) -- lt' = lt
                           , (n_rtvar' v, rtvar v) -- rt' = rt
                           ]
                         -- variables not updated by a1 stay the same
                         | v <- (a2^.aSt^.ports) \\ (a1^.aSt^.ports) 
                         ]
    updates2_2 = [ lukap v
                 | p <- (a2^.aSt^.ports) `intersect` (a1^.aSt^.ports) 
                 , v <- primes p
                 ]
    
    body   = Ands [ prevKV a1
                  , prevKV a2
                  , sanGlobs a1, taintEqs a1
                  , sanGlobs a2, taintEqs a2
                  , nl1
                  , nr1
                  ]

provedProperty :: PropertyOptions -> AlwaysBlock -> [Inv]
provedProperty (PropertyOptions{..}) a = 
  if checkTagEq then tagEq else [] ++
  if checkValEq then valEq else []
  where
    tagEq = [ Horn { hHead = BinOp GE (rtvar s) (Number 1)
                   , hBody = Ands [ KV { kvId   = a ^. aId
                                       , kvSubs = [ (n_rtvar' s, rtvar s)
                                                  , (n_ltvar' s, ltvar s)
                                                  ]
                                       }
                                  , BinOp GE (ltvar s) (Number 1)
                                  ]
                   , hId   = HornId (a ^. aId) InvTagEq
                   }
            | s <- a^.aSt^.sinks
            ]
    valEq = [ Horn { hHead =  BinOp EQU (lvar s) (rvar s)
                   , hBody = Ands [ KV { kvId   = a ^. aId
                                       , kvSubs = [ (n_lvar' s, lvar s)
                                                  , (n_rvar' s, rvar s)
                                                  ]
                                       }
                                  ]
                   , hId   = HornId (a ^. aId) (InvOther "l_sink=r_sink")
                   }
            | s <- a^.aSt^.sinks
            ]

-------------------------------------------------------------------------------- 
-- Helper functions
-------------------------------------------------------------------------------- 
lvar, rvar, ltvar, rtvar, rvar' :: Id -> Expr
lvar  = makeVar fmt{leftVar=True}
rvar  = makeVar fmt{rightVar=True}
ltvar = makeVar fmt{taggedVar=True, leftVar=True}
rtvar = makeVar fmt{taggedVar=True, rightVar=True}

rvar'  = makeVar fmt{primedVar=True, rightVar=True}

-- lvar', ltvar', rtvar' :: Id -> Expr
-- lvar'  = makeVar fmt{primedVar=True, leftVar=True}
-- ltvar' = makeVar fmt{primedVar=True, taggedVar=True, leftVar=True}
-- rtvar' = makeVar fmt{primedVar=True, taggedVar=True, rightVar=True}

n_lvar', n_rvar', n_ltvar', n_rtvar' :: Id -> Id
n_lvar'  = makeVarName fmt{primedVar=True, leftVar=True}
n_rvar'  = makeVarName fmt{primedVar=True, rightVar=True}
n_ltvar' = makeVarName fmt{primedVar=True, taggedVar=True, leftVar=True}
n_rtvar' = makeVarName fmt{primedVar=True, taggedVar=True, rightVar=True}

-- n_lvar, n_rvar, n_ltvar, n_rtvar :: Id -> Id
-- n_lvar   = makeVarName fmt{leftVar=True}
-- n_rvar   = makeVarName fmt{rightVar=True}
-- n_ltvar  = makeVarName fmt{taggedVar=True, leftVar=True}
-- n_rtvar  = makeVarName fmt{taggedVar=True, rightVar=True}


-- kv[x' := x][y' := y][...]
prevKV   :: AlwaysBlock -> Expr
prevKV a = KV { kvId   = a^.aId
              , kvSubs = subs
              } 
  where
    args  = makeInvArgs fmt a
    args' = makeInvArgs fmt{primedVar=True} a
    subs  = zipWith (\ v v' -> (v', Var v)) args args'

primes :: Id -> [Id]
primes v = [ makeVarName f v
           | f <- [ f'{leftVar=True}
                  , f'{rightVar=True}
                  , f'{taggedVar=True, leftVar=True}
                  , f'{taggedVar=True, rightVar=True}
                  ]
           ]
  where
    f' = fmt{primedVar=True}

