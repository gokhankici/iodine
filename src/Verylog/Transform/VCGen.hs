{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

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
import qualified Data.Sequence            as SQ
import qualified Data.Foldable            as F

type ABS = SQ.Seq AlwaysBlock

--------------------------------------------------------------------------------
invs :: AnnotSt -> ABS -> Constraints
--------------------------------------------------------------------------------
invs annots as = cs2
  where
    cs1 = F.foldl' go mempty as
    cs2 = F.foldl' go2 cs1 nics

    nics = non_interference_checks annots as

    go2 c (id1, _, i) =
      let f Nothing   = Just $ return i
          f (Just is) = Just $ i SQ.<| is
      in  IM.alter f id1 c

    go c a = IM.insert (a^.aId) is c
      where
        is = modular_inv annots a SQ.><
             provedProperty annots defaultPropertyOptions a

--------------------------------------------------------------------------------
modular_inv :: AnnotSt -> AlwaysBlock -> SQ.Seq Inv
--------------------------------------------------------------------------------
modular_inv annots a =
  initial_inv   annots a' SQ.<|
  tag_reset_inv annots a' SQ.<|
  src_reset_inv annots a' SQ.<|
  next_step_inv annots a' SQ.<|
  mempty
  where
    a' = dbg (printf "\nalways block #%d:\n%s" (a^.aId) (show a)) a

--------------------------------------------------------------------------------
initial_inv :: AnnotSt -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
initial_inv annots a =
  Horn { hBody = Boolean True
       , hHead = Ands [ KV { kvId   = a ^. aId
                           , kvSubs = sub1 SQ.>< sub2
                           }
                      ]
       , hId   = HornId i (InvInit i)
       }
  where
    i    = a ^. aId
    sub1 = S.foldl' (\acc s -> acc SQ.|> (n_lvar s, rvar s)) mempty (annots^.sanitize)
    sub2 = (\t -> (t,Boolean False)) <$> makeInvTags fmt a


--------------------------------------------------------------------------------
tag_reset_inv :: AnnotSt -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
tag_reset_inv annots a =
  Horn { hBody =  prevKV a
       , hHead = KV { kvId   = i
                    , kvSubs = subs
                    }
       , hId   = HornId i (InvReTag i)
       }
  where
    i          = a ^. aId
    srcs       = annots^.sources
    subs       = F.foldl' go mempty (a^.aSt^.ports)
    mk b t     = (t, Boolean b)
    go acc var =
      if | n `S.member` srcs -> (mk True  <$> makeBothTag n) SQ.>< acc
         | isRegister var    -> (mk False <$> makeBothTag n) SQ.>< acc
         | otherwise         -> acc
      where n :: Id = varName var


--------------------------------------------------------------------------------
src_reset_inv :: AnnotSt -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
src_reset_inv annots a =
  Horn { hBody =  prevKV a
       , hHead = KV { kvId   = i
                    , kvSubs = tags
                    }
       , hId   = HornId i (InvSrcReset i)
       }
  where
    i     = a ^. aId
    srcs  = srcLs annots
    tags  = (\v -> (v, Boolean False)) <$> makeBothTags srcs

--------------------------------------------------------------------------------
next_step_inv :: AnnotSt -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
next_step_inv annots a =
  Horn { hBody = body
       , hHead = KV { kvId   = i
                    , kvSubs = subs
                    }
       , hId   = HornId i (InvNext i)
       }
  where
    i        = a ^. aId
    subs     = ul SQ.>< ur
    (nl,ul)  = next fmt{leftVar=True}  a
    (nr,ur)  = next fmt{rightVar=True} a
    body     = Ands [ prevKV a
                    , sanGlobs (annots^.sanitizeGlob) subs
                    , taintEqs (annots^.taintEq) subs
                    , sourcesAreEqual $ F.toList srcs
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


type Subs = SQ.Seq (Id,Expr)
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
        in case (mylookup vl subs, mylookup vr subs) of
             (Just el, Just er) -> Just (el, er, o)
             _                  -> Nothing
      | f <- fmts
      ]



type NIC  = (Int, Int, Inv)
type NICs = SQ.Seq NIC
--------------------------------------------------------------------------------
non_interference_checks :: AnnotSt -> ABS -> NICs
--------------------------------------------------------------------------------
non_interference_checks annots as = non_int_chk as mempty mempty
  where
    hasCommon :: S.HashSet Id -> S.HashSet Id -> Bool
    hasCommon s1 s2 = not . null $ S.intersection s1 s2

    non_int_chk :: ABS -> ABS -> NICs -> NICs
    non_int_chk ass checked cs =
      case SQ.viewl ass of
        SQ.EmptyL    -> cs
        a1 SQ.:< a1s ->
          let cs' = F.foldl' f cs checked
              r1  = a1 ^. aMd ^. mRegReadSet
              w1  = a1 ^. aMd ^. mRegWriteSet
              f cs_prev a2 =
                let i1 = a1 ^. aId
                    i2 = a2 ^. aId
                    r2 :: S.HashSet Id = a2 ^. aMd ^. mRegReadSet
                    w2 :: S.HashSet Id = a2 ^. aMd ^. mRegWriteSet
                in
                if   hasCommon w1 w2
                then cs_prev SQ.|>
                     (i1, i2, non_interference_inv annots a1 a2) SQ.|>
                     (i2, i1, non_interference_inv annots a2 a1)
                else let t12 = if   hasCommon w1 r2
                               then cs_prev SQ.|> (i1, i2, non_interference_inv annots a1 a2)
                               else cs_prev
                     in  if   hasCommon w2 r1
                         then t12 SQ.|> (i2, i1, non_interference_inv annots a2 a1)
                         else t12
          in non_int_chk a1s (checked SQ.|> a1) cs'

--------------------------------------------------------------------------------
non_interference_inv :: AnnotSt -> AlwaysBlock -> AlwaysBlock -> Inv
--------------------------------------------------------------------------------
-- when a1 takes a step, a2 still holds
non_interference_inv annots a1 a2 =
  Horn { hBody = body
       , hHead = KV { kvId   = a2 ^. aId
                    , kvSubs = updates2
                    }
       , hId   = HornId (a2 ^. aId) (InvInter (a1 ^. aId))
       }
  where
    (nl1,ul1) = next fmt{leftVar=True}  a1
    (nr1,ur1) = next fmt{rightVar=True} a1
    updates1  = ul1 SQ.>< ur1
    lukap v   = case mylookup v updates1 of
                  Nothing -> throw $ PassError $ "cannot find " ++ id2Str v ++ " in updates1"
                  Just e  -> (v,e)
    ps1 = seq2set $ a1 ^. aSt ^. ports
    ps2 = seq2set $ a2 ^. aSt ^. ports
    updates2    = updates2_1 SQ.>< updates2_2

    updates2_1 =
      S.foldl'
      (\acc var -> let v :: Id = varName var in
          (n_lvar v,  lvar v) SQ.<|  -- l' = l
          (n_rvar v,  rvar v) SQ.<|  -- r' = r
          (n_ltvar v, ltvar v) SQ.<| -- lt' = lt
          (n_rtvar v, rtvar v) SQ.<| -- rt' = rt
          acc
      )
      mempty
      (ps2 `S.difference` ps1)  -- variables not updated by a1 stay the same

    updates2_2 =
      S.foldl'
      (\acc var -> let p :: Id = varName var in
          foldl' (\acc2 v -> lukap v SQ.<| acc2) acc (primes p)
      )
      mempty
      (ps2 `S.intersection` ps1)


    body   = Ands [ prevKV a1
                  , prevKV a2
                  , sanGlobs (annots^.sanitizeGlob) (updates1 SQ.>< updates2)
                  , taintEqs (annots^.taintEq) (updates1 SQ.>< updates2)
                  , sourcesAreEqual $ S.toList (annots^.sources)
                  , nl1
                  , nr1
                  ]

--------------------------------------------------------------------------------
provedProperty :: AnnotSt -> PropertyOptions -> AlwaysBlock -> SQ.Seq Inv
--------------------------------------------------------------------------------
provedProperty annots (PropertyOptions{..}) a =
  (if checkTagEq then tagEq else mempty) SQ.><
  (if checkValEq then valEq else mempty) SQ.><
  assertEqs
  where
    i = a ^. aId

    tagEq =
      F.foldl'
      (\acc s ->
         if   isReg a s
         then Horn { hHead = BinOp IFF (ltvar s) (rtvar s)
                   , hBody = KV { kvId   = i
                                , kvSubs = (n_rtvar s, rtvar s) SQ.<|
                                           (n_ltvar s, ltvar s) SQ.<|
                                           mempty
                                }
                   , hId   = HornId i (InvTagEq i)
                   } SQ.<| acc
         else acc
      ) mempty (annots^.sinks)

    valEq =
      F.foldl'
      (\acc s ->
         if   isReg a s
         then Horn { hHead =  BinOp EQU (lvar s) (rvar s)
                   , hBody = Ands [ KV { kvId   = i
                                       , kvSubs = (n_lvar s, lvar s) SQ.<|
                                                  (n_rvar s, rvar s) SQ.<|
                                                  mempty
                                       }
                                  ]
                   , hId   = HornId i (InvOther "l_sink=r_sink")
                   } SQ.<| acc
         else acc
      ) mempty (annots^.sinks)

    assertEqs =
      F.foldl'
      (\acc s ->
         if isReg a s
         then Horn { hHead =  BinOp EQU (lvar s) (rvar s)
                   , hBody = Ands [ KV { kvId   = i
                                       , kvSubs = (n_lvar s, lvar s) SQ.<|
                                                  (n_rvar s, rvar s) SQ.<|
                                                  mempty
                                       }
                                  ]
                   , hId   = HornId i (InvOther "left var = right var")
                   } SQ.<| acc
         else acc
      ) mempty (annots^.assertEq)

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
              , kvSubs = mempty
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

srcLs :: AnnotSt -> SQ.Seq Id
srcLs = view (sources . to f2seq)

mylookup :: (Eq a) => a -> SQ.Seq (a, b) -> Maybe b
mylookup a = go
  where
    go s =
      case SQ.viewl s of
        SQ.EmptyL        -> Nothing
        (a', b) SQ.:< s' -> if   a == a'
                            then Just b
                            else go s'
