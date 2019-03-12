{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.VCGen ( invs
                               ) where

import Verylog.Language.Types
import Verylog.Solver.Common
import Verylog.Transform.TransitionRelation
import Verylog.Transform.Utils              as U


import           Control.Exception
import           Control.Lens
import           Data.Maybe
import           Data.List
import qualified Data.HashSet             as S
import qualified Data.IntMap.Strict       as IM
import           Text.Printf

--------------------------------------------------------------------------------
invs :: AnnotSt -> [AlwaysBlock] -> Constraints
--------------------------------------------------------------------------------
invs annots as = cs2
  where
    cs1 = foldl' go mempty as
    cs2 = foldl' go2 cs1 nics

    nics = non_interference_checks annots as

    go2 c (id1, _, i) =
      let f Nothing   = Just [i]
          f (Just is) = Just (i:is)
      in  IM.alter f id1 c

    go c a = IM.insert (a^.aId) is c
      where
        is = modular_inv annots a ++
             provedProperty annots defaultPropertyOptions a

--------------------------------------------------------------------------------
modular_inv :: AnnotSt -> AlwaysBlock -> [Inv]
--------------------------------------------------------------------------------
modular_inv annots a =
  [ initial_inv   annots a'
  , tag_reset_inv annots a'
  , src_reset_inv annots a'
  , next_step_inv annots a'
  ]
  where
    a' = dbg (printf "\nalways block #%d:\n%s" (a^.aId) (show a)) a

--------------------------------------------------------------------------------
initial_inv :: AnnotSt -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
initial_inv annots a =
  Horn { hBody = Boolean True
       , hHead = Ands [ KV { kvId   = a ^. aId
                           , kvSubs = filterSubs a (sub1 ++ sub2)
                                      ++ [ (v, Boolean False) | v <- makeBothTags srcs]
                           }
                      ]
       , hId   = HornId i (InvInit i)
       }
  where
    srcs = srcLs annots
    i      = a ^. aId
    sub1 = [ (n_lvar sntz, rvar sntz)
           | sntz <- S.toList $ annots ^. sanitize
           ]
    sub2 = [ (t, Boolean False)
           | t <- makeInvTags fmt a
           ]

--------------------------------------------------------------------------------
tag_reset_inv :: AnnotSt -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
tag_reset_inv annots a =
  Horn { hBody =  prevKV a
       , hHead = KV { kvId   = i
                    , kvSubs = filterSubs a hsubs
                               ++ [ (v, Boolean True) | v <- makeBothTags srcs]
                    }
       , hId   = HornId i (InvReTag i)
       }
  where
    srcs = srcLs annots
    i      = a ^. aId
    hsubs  = [(t, Boolean False) | t <- makeBothTags $ (getRegisters a \\ srcs)]


--------------------------------------------------------------------------------
src_reset_inv :: AnnotSt -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
src_reset_inv annots a =
  Horn { hBody =  prevKV a
       , hHead = KV { kvId   = i
                    , kvSubs = [ (v, Boolean False) | v <- makeBothTags srcs]
                    }
       , hId   = HornId i (InvSrcReset i)
       }
  where
    srcs = srcLs annots
    i = a ^. aId

--------------------------------------------------------------------------------
next_step_inv :: AnnotSt -> AlwaysBlock -> Inv 
--------------------------------------------------------------------------------
next_step_inv annots a =
  Horn { hBody = body
       , hHead = KV { kvId   = i
                    , kvSubs = filterSubs a subs
                    }
       , hId   = HornId i (InvNext i)
       }
  where
    i        = a ^. aId
    subs     = ul ++ ur
    (nl,ul)  = next fmt{leftVar=True}  a
    (nr,ur)  = next fmt{rightVar=True} a
    body     = Ands [ prevKV a
                    , sanGlobs (annots^.sanitizeGlob) subs
                    , taintEqs (annots^.taintEq) subs
                    , sourcesAreEqual srcs
                    , nl, nr
                    ]
    srcs = srcLs annots

-- wire input sources
sourcesAreEqual :: [Id] -> Expr
sourcesAreEqual srcs = Ands $ h <$> twoPairs srcs
  where
    h (x,y)  = let fl = fmt{taggedVar=True, leftVar=True}
                   fr = fmt{taggedVar=True, rightVar=True}
                   xl = makeVar fl x
                   xr = makeVar fr x
                   yl = makeVar fl y
                   yr = makeVar fr y
               in  Ands [ BinOp IFF xl yr
                        , BinOp IFF xr yl
                        , BinOp IFF xl xr
                        , BinOp IFF yl yr
                        ]


type Subs = [(Id,Expr)]
type Ids = S.HashSet Id          -- annotation set

-- sanitize globs are always the same
sanGlobs        :: Ids -> Subs -> Expr
sanGlobs vs subs = alwaysEqs conf vs subs
  where
    conf = AEC { isInitEq  = True
               , isPrimeEq = True
               , isValEq   = True
               , isTagEq   = True
               }

taintEqs        :: Ids -> Subs -> Expr
taintEqs vs subs = alwaysEqs conf vs subs
  where
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

alwaysEqs :: AlwaysEqConfig -> Ids -> Subs -> Expr
alwaysEqs (AEC{..}) vs subs = Ands (initEq ++ primeEq)
  where
    fmts :: [VarFormat]
    fmts = (if isValEq then [fmt]                 else []) ++
           (if isTagEq then [fmt{taggedVar=True}] else [])
             
    initEq :: [Expr]
    initEq  =
      if   isInitEq
      then [ let o = if taggedVar f then IFF else EQU
             in  BinOp o
                 (makeVar f{leftVar=True} v)
                 (makeVar f{rightVar=True} v)
           | v <- S.toList vs, f <- fmts
           ]
      else []

    primeEq :: [Expr]
    primeEq =
      if   isPrimeEq
      then [ BinOp o exprL exprR
           | v <- S.toList vs, (exprL, exprR, o) <- findLastIfExists v
           ]
      else []

    findLastIfExists :: Id -> [(Expr, Expr, BinOp)]
    findLastIfExists v =
      catMaybes
      [ let vl = makeVarName f{leftVar=True} v
            vr = makeVarName f{rightVar=True} v
            o  = if taggedVar f then IFF else EQU
        in case (lookup vl subs, lookup vr subs) of
             (Just el, Just er) -> Just (el, er, o)
             _                  -> Nothing
      | f <- fmts
      ]
  
type NIC = (Int, Int, Inv)
--------------------------------------------------------------------------------
non_interference_checks :: AnnotSt -> [AlwaysBlock] -> [NIC]
--------------------------------------------------------------------------------
non_interference_checks annots as = non_int_chk as [] []
  where
    hasCommon :: S.HashSet Id -> S.HashSet Id -> Bool
    hasCommon s1 s2 = not . null $ S.intersection s1 s2  

    non_int_chk :: [AlwaysBlock] -> [AlwaysBlock] -> [NIC] -> [NIC]
    non_int_chk []      _checked cs = cs
    non_int_chk (a1:a1s) checked cs =
      let cs' = foldl' f cs checked
          r1  = a1 ^. aMd ^. mRegReadSet
          w1  = a1 ^. aMd ^. mRegWriteSet
          f cs_prev a2 =
            let i1 = a1 ^. aId
                i2 = a2 ^. aId
                r2 = a2 ^. aMd ^. mRegReadSet
                w2 = a2 ^. aMd ^. mRegWriteSet
            in
            if   hasCommon w1 w2
            then (i1, i2, non_interference_inv annots a1 a2) :
                 (i2, i1, non_interference_inv annots a2 a1) :
                 cs_prev
            else let t12 = if   hasCommon w1 r2
                           then (i1, i2, non_interference_inv annots a1 a2) : cs_prev
                           else cs_prev
                 in  if   hasCommon w2 r1
                     then (i2, i1, non_interference_inv annots a2 a1) : t12
                     else t12
      in non_int_chk a1s (a1:checked) cs'

--------------------------------------------------------------------------------
non_interference_inv :: AnnotSt -> AlwaysBlock -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
-- when a1 takes a step, a2 still holds
non_interference_inv annots a1 a2 =
  Horn { hBody = body
       , hHead = KV { kvId   = a2 ^. aId
                    , kvSubs = filterSubs a2 updates2
                    }
       , hId   = HornId (a2 ^. aId) (InvInter (a1 ^. aId))
       }
  where
    (nl1,ul1) = next fmt{leftVar=True}  a1
    (nr1,ur1) = next fmt{rightVar=True} a1
    updates1  = ul1 ++ ur1
    lukap v   = case lookup v updates1 of
                  Nothing -> throw $ PassError $ "cannot find " ++ id2Str v ++ " in updates1"
                  Just e  -> (v,e)
    updates2    = updates2_1 ++ updates2_2
    updates2_1  = concat [ [ (n_lvar v,  lvar v)  -- l' = l
                           , (n_rvar v,  rvar v)  -- r' = r
                           , (n_ltvar v, ltvar v) -- lt' = lt
                           , (n_rtvar v, rtvar v) -- rt' = rt
                           ]
                         -- variables not updated by a1 stay the same
                         --- | v <- (getRegisters a2) \\ (getRegisters a1) 
                         | v <- varName <$> ((a2 ^. aSt ^. ports) \\ (a1 ^. aSt ^. ports)) 
                         ]
    updates2_2 = [ lukap v
                 --- | p <- (getRegisters a2) `intersect` (getRegisters a1) 
                 | p <- varName <$> ((a2 ^. aSt ^. ports) `intersect` (a1 ^. aSt ^. ports)) 
                 , v <- primes p
                 ]
    
    body   = Ands [ prevKV a1
                  , prevKV a2
                  , sanGlobs (annots^.sanitizeGlob) (updates1++updates2)
                  , taintEqs (annots^.taintEq) (updates1++updates2)
                  , sourcesAreEqual $ srcLs annots
                  , nl1
                  , nr1
                  ]

--------------------------------------------------------------------------------
provedProperty :: AnnotSt -> PropertyOptions -> AlwaysBlock -> [Inv]
--------------------------------------------------------------------------------
provedProperty annots (PropertyOptions{..}) a = 
  (if checkTagEq then tagEq else []) ++
  (if checkValEq then valEq else []) ++
  assertEqs
  where
    i     = a ^. aId
    tagEq = [ Horn { hHead = BinOp IFF (ltvar s) (rtvar s)
                   , hBody = KV { kvId   = i
                                , kvSubs = [ (n_rtvar s, rtvar s)
                                           , (n_ltvar s, ltvar s)
                                           ]
                                }
                   , hId   = HornId i (InvTagEq i)
                   }
            | s <- S.toList $ S.filter (isReg a) (annots^.sinks)
            ]
    valEq = [ Horn { hHead =  BinOp EQU (lvar s) (rvar s)
                   , hBody = Ands [ KV { kvId   = i
                                       , kvSubs = [ (n_lvar s, lvar s)
                                                  , (n_rvar s, rvar s)
                                                  ]
                                       }
                                  ]
                   , hId   = HornId i (InvOther "l_sink=r_sink")
                   }
            | s <- S.toList $ S.filter (isReg a) (annots^.sinks)
            ]
    assertEqs =
      [ Horn { hHead =  BinOp EQU (lvar s) (rvar s)
             , hBody = Ands [ KV { kvId   = i
                                 , kvSubs = [ (n_lvar s, lvar s)
                                            , (n_rvar s, rvar s)
                                            ]
                                 }
                            ]
             , hId   = HornId i (InvOther "left var = right var")
             }
      | s <- S.toList $ S.filter (isReg a) (annots^.assertEq)
      ]

data PropertyOptions = PropertyOptions { checkTagEq :: Bool
                                       , checkValEq :: Bool
                                       }

defaultPropertyOptions :: PropertyOptions
defaultPropertyOptions = PropertyOptions { checkTagEq = True
                                         , checkValEq = False
                                         }


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

isReg :: AlwaysBlock -> Id -> Bool
isReg a v = (Register v) `elem` (a ^. aSt ^. ports)

-- TODO : is this not necessary ?
filterSubs :: AlwaysBlock -> [(Id,Expr)] -> [(Id,Expr)]
filterSubs _ = id

srcLs :: AnnotSt -> [Id]
srcLs = view (sources . to S.toList)
