{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.VCGen ( invs
                               ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.List
import qualified Data.HashSet             as S
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
invs as =
  concatMap modular_inv as
  ++ non_interference_checks as
  ++ concatMap (provedProperty defaultPropertyOptions) as

--------------------------------------------------------------------------------
modular_inv :: AlwaysBlock -> [Inv]  
--------------------------------------------------------------------------------
modular_inv a = [initial_inv, tag_reset_inv, next_step_inv] <*> [a']
  where
    a' = a
    -- a' = trc (printf "\nalways block #%d:\n" (a^.aId)) a a
    -- a' = trc (printf "\nalways block #%d: " (a^.aId)) (length $ makeInvArgs fmt a) a

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
    sub1 = [ (n_lvar sntz, rvar sntz)
           | sntz <- S.toList . S.fromList . (filterRegs a) $
                     st ^. sanitize ++ st ^. sources ++ st ^. sinks
           ]
    sub2 = [ (tv, Number 0)
           | s <- getRegisters a, tv <- [n_ltvar, n_rtvar] <*> [s]
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

    hsubs  = hsubs1 ++ hsubs2 -- ++ hsubs3
    hsubs1 = concat [ [ (n_lvar p, lvar p)
                      , (n_rvar p, rvar p)
                      ]
                    | p <- getRegisters a
                    ]
    hsubs2 = [ (tv, Number 1)
             | s <- filterRegs a (st^.sources)
             , tv <- [n_ltvar, n_rtvar] <*> [s]
             ]
    -- hsubs3 = [ (tv, Number 0)
    --          | v <- (varName <$> st^.ports) \\ (st^.sources)
    --          , tv <- [n_ltvar, n_rtvar] <*> [v]
    --          ]

--------------------------------------------------------------------------------
next_step_inv :: AlwaysBlock -> Inv 
--------------------------------------------------------------------------------
next_step_inv a = Horn { hBody = body
                       , hHead = KV { kvId   = a^.aId
                                    , kvSubs = subs
                                    }
                       , hId   = HornId (a ^. aId) InvNext
                       }
  where
    subs     = filterSubs a (ul ++ ur)
    (nl,ul)  = next fmt{leftVar=True}  a
    (nr,ur)  = next fmt{rightVar=True} a
    body     = Ands [ prevKV a
                    , sanGlobs a subs, taintEqs a subs
                    , nl, nr
                    ]

type Subs = [(Id,Expr)]

-- sanitize globs are always the same
sanGlobs        :: AlwaysBlock -> Subs -> Expr
sanGlobs a subs = alwaysEqs conf vs subs
  where
    vs   = (getRegisters a) `intersect` (a ^. aSt ^. sanitizeGlob)
    conf = AEC { isInitEq  = True
               , isPrimeEq = True
               , isValEq   = True
               , isTagEq   = True
               }

taintEqs        :: AlwaysBlock -> Subs -> Expr
taintEqs a subs = alwaysEqs conf vs subs
  where
    vs   = (getRegisters a) `intersect` (a ^. aSt ^. taintEq)
    conf = AEC { isInitEq  = True
               , isPrimeEq = True
               , isValEq   = False
               , isTagEq   = True
               }

data AlwaysEqConfig = AEC { isInitEq  :: Bool
                          , isPrimeEq :: Bool
                          , isValEq   :: Bool
                          , isTagEq   :: Bool
                          }

alwaysEqs :: AlwaysEqConfig -> [Id] -> Subs -> Expr
alwaysEqs (AEC{..}) vs subs = Ands (initEq ++ primeEq)
  where
    fmts :: [VarFormat]
    fmts = (if isValEq then [fmt]                 else []) ++
           (if isTagEq then [fmt{taggedVar=True}] else [])
             
    initEq :: [Expr]
    initEq  =
      if   isInitEq
      then [ BinOp EQU 
             (makeVar f{leftVar=True} v)
             (makeVar f{rightVar=True} v)
           | v <- vs, f <- fmts
           ]
      else []

    primeEq :: [Expr]
    primeEq =
      if   isPrimeEq
      then [ BinOp EQU exprL exprR
           | v <- vs, (exprL, exprR) <- findLasts v
           ]
      else []

    findLasts :: Id -> [(Expr, Expr)]
    findLasts v =
      [ let vl = makeVarName f{leftVar=True} v
            vr = makeVarName f{rightVar=True} v
        in case (lookup vl subs, lookup vr subs) of
             (Just el, Just er) -> (el, er)
             _                  -> error $
                                   printf "findLasts failed. \n  vl: %s\n  vr: %s\n  subs: %s\n"
                                   vl vr (show subs)
                                   
      | f <- (\f -> f) <$> fmts
      ]
  
  
--------------------------------------------------------------------------------
non_interference_checks :: [AlwaysBlock] -> [Inv]
--------------------------------------------------------------------------------
non_interference_checks as = non_int_chk as [] []
  where
    interfere :: RWSet -> RWSet -> Bool
    interfere (r1,w1) (r2,w2) = notDistinct r1 w2 || notDistinct r2 w1 || notDistinct w1 w2

    notDistinct :: S.HashSet Id -> S.HashSet Id -> Bool
    notDistinct s1 s2 = not . null $ S.intersection s1 s2  

    non_int_chk :: [AlwaysBlock] -> [(RWSet,AlwaysBlock)] -> [Inv] -> [Inv]
    non_int_chk []      _checked cs = cs
    non_int_chk (a1:a1s) checked cs =
      let f cs_prev (rw2, a2) = if   interfere rw1 rw2
                                then (non_interference_inv a1 a2)
                                     : (non_interference_inv a2 a1)
                                     : cs_prev
                                else cs_prev
          cs'                 = foldl' f cs checked
          rw1                 = readWriteSet a1
      in non_int_chk a1s ((rw1, a1):checked) cs'

type RWSet = (S.HashSet Id, S.HashSet Id)

readWriteSet :: AlwaysBlock -> RWSet
readWriteSet a = evalState (comp (a^.aStmt) >> get) (S.empty, S.empty)
  where
    us = a^.aSt^.ufs

    readVars  v = S.fromList . filterRegs a $ M.lookupDefault [v] v us
    writeVars v = S.fromList . filterRegs a $ if M.member v us then [] else [v]
    
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
                                               , kvSubs = updates2
                                               }
                                  , hId   = HornId (a2 ^. aId) (InvInter (a1 ^. aId))
                                  }
  where
    (nl1,ul1) = next fmt{leftVar=True}  a1
    (nr1,ur1) = next fmt{rightVar=True} a1
    updates1  = ul1 ++ ur1
    lukap v   = case lookup v updates1 of
                  Nothing -> throw $ PassError $ "cannot find " ++ v ++ " in updates1"
                  Just e  -> (v,e)
    updates2    = filterSubs a2 $ updates2_1 ++ updates2_2
    updates2_1  = concat [ [ (n_lvar v,  lvar v)  -- l' = l
                           , (n_rvar v,  rvar v)  -- r' = r
                           , (n_ltvar v, ltvar v) -- lt' = lt
                           , (n_rtvar v, rtvar v) -- rt' = rt
                           ]
                         -- variables not updated by a1 stay the same
                         | v <- (getRegisters a2) \\ (getRegisters a1) 
                         ]
    updates2_2 = [ lukap v
                 | p <- (getRegisters a2) `intersect` (getRegisters a1) 
                 , v <- primes p
                 ]
    
    body   = Ands [ prevKV a1
                  , prevKV a2
                  , sanGlobs a1 updates1, taintEqs a1 updates1
                  , sanGlobs a2 updates2, taintEqs a2 updates2
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
                                       , kvSubs = [ (n_rtvar s, rtvar s)
                                                  , (n_ltvar s, ltvar s)
                                                  ]
                                       }
                                  , BinOp GE (ltvar s) (Number 1)
                                  ]
                   , hId   = HornId (a ^. aId) InvTagEq
                   }
            | s <- filterRegs a $ a^.aSt^.sinks
            ]
    valEq = [ Horn { hHead =  BinOp EQU (lvar s) (rvar s)
                   , hBody = Ands [ KV { kvId   = a ^. aId
                                       , kvSubs = [ (n_lvar s, lvar s)
                                                  , (n_rvar s, rvar s)
                                                  ]
                                       }
                                  ]
                   , hId   = HornId (a ^. aId) (InvOther "l_sink=r_sink")
                   }
            | s <- filterRegs a $ a^.aSt^.sinks
            ]

-------------------------------------------------------------------------------- 
-- Helper functions
-------------------------------------------------------------------------------- 
lvar, rvar, ltvar, rtvar :: Id -> Expr
lvar  = makeVar fmt{leftVar=True}
rvar  = makeVar fmt{rightVar=True}
ltvar = makeVar fmt{taggedVar=True, leftVar=True}
rtvar = makeVar fmt{taggedVar=True, rightVar=True}

n_lvar, n_rvar, n_ltvar, n_rtvar :: Id -> Id
n_lvar   = makeVarName fmt{leftVar=True}
n_rvar   = makeVarName fmt{rightVar=True}
n_ltvar  = makeVarName fmt{taggedVar=True, leftVar=True}
n_rtvar  = makeVarName fmt{taggedVar=True, rightVar=True}

prevKV   :: AlwaysBlock -> Expr
prevKV a = KV { kvId   = a^.aId
              , kvSubs = []
              } 
primes :: Id -> [Id]
primes v = [ makeVarName f v
           | f <- [ f'{leftVar=True}
                  , f'{rightVar=True}
                  , f'{taggedVar=True, leftVar=True}
                  , f'{taggedVar=True, rightVar=True}
                  ]
           ]
  where
    -- f' = fmt{primedVar=True}
    f' = fmt

filterRegs :: AlwaysBlock -> [Id] -> [Id]
filterRegs a vs =
  foldl' (\l v -> if   (Register v) `elem` (a ^. aSt ^. ports)
                  then v:l
                  else l
         ) [] vs

kv_vars :: AlwaysBlock -> S.HashSet Id
kv_vars a =
  (S.fromList (allArgs fmt{leftVar=True} (a^.aSt)))
  `S.union`
  (S.fromList (allArgs fmt{rightVar=True} (a^.aSt)))

filterSubs :: AlwaysBlock -> [(Id,Expr)] -> [(Id,Expr)]
filterSubs a = filter (\(v,_) -> v `S.member` m)
  where
    m = kv_vars a
