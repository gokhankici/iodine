{-# LANGUAGE RecordWildCards #-}

module Verylog.Transform.FP.VCGen ( toFpSt
                                  , toFpSt'
                                  ) where

import           Verylog.Language.Types
import           Verylog.Solver.Common
import           Verylog.Solver.FP.Types
import           Verylog.Transform.Utils
import           Verylog.Transform.VCGen

import           Control.Monad.State.Lazy
import           Control.Lens
import           Data.List
import qualified Data.HashMap.Strict      as M
import qualified Data.Sequence            as SQ
import qualified Data.Foldable            as F
import qualified Data.IntMap.Strict       as IM
import qualified Language.Fixpoint.Types  as FQ

-- import Debug.Trace

type ABS = SQ.Seq AlwaysBlock
type S = State (Int, BindMap)

toFpSt' :: AnnotSt -> FPSt -> FPSt
toFpSt' newAnnots st =
  set fpConstraints cs .
  set fpAnnotations newAnnots $
  st
  where
    cs = invs newAnnots (st^.fpABs)

toFpSt  :: (ABS, (AnnotSt, [FPQualifier])) -> FPSt
toFpSt (_as, (allAnnots, allQualifiers)) =
  FPSt { _fpConstraints = cs
       , _fpABs         = as
       , _fpBinds       = bs'
       , _fpQualifiers  = allQualifiers
       , _fpAnnotations = allAnnots
       }
  where
    -- as  = updateFPVars <$> _as
    as = _as
    cs  = invs allAnnots as
    bs  = getBinds as $ mconcat $ IM.elems cs
    ni  = M.size bs + 1
    ts  = makeBothTags $
          foldl' (\acc q -> SQ.fromList (qualifVars q) SQ.>< acc) mempty allQualifiers
    bs' = foldl' (\m (n,name) -> h m n name) bs (zip [ni..] (F.toList ts))
    h m i name =
      let f v Nothing  = Just v
          f _ (Just v) = Just v
      in  M.alter (f FQBind { bindId   = i
                            , bindName = name
                            , bindType = FQ.FTC FQ.boolFTyCon
                            , bindRef  = FQ.PTrue
                            }) name m


getBinds :: ABS -> SQ.Seq Inv -> BindMap
getBinds as cs = evalState comp (length constants + 1, m)
  where
    comp = do sequence_ (getBind <$> cs)
              let is = (makeInvArgs fmt <$> as)
              getBindsFromExps $ fmap Var $ F.foldl' (SQ.><) mempty is
              use _2

    m = foldr constBind M.empty (zip [1..] constants)
    h (Number n)      = FQ.ECon (FQ.I (toInteger n))
    h (Boolean True)  = FQ.PTrue
    h (Boolean False) = FQ.PFalse
    h _               = error "oh no!"
    constBind (i, (name, e)) =
      M.insert name FQBind { bindId   = i
                           , bindName = name
                           , bindType = case e of
                                          Boolean _ -> FQ.FTC FQ.boolFTyCon
                                          _         -> FQ.FInt
                           , bindRef  = (case e of
                                          Boolean _ -> FQ.PIff
                                          _         -> FQ.PAtom FQ.Eq)
                                        (FQ.EVar $ FQ.symbol "v")
                                        (h e)
                           }

getBind            :: Inv -> S ()
getBind (Horn{..}) = getBindsFromExps [hBody, hHead]

getBindsFromExp :: Expr -> S ()
getBindsFromExp (BinOp{..}) = getBindsFromExps [expL, expR]
getBindsFromExp (Ands es)   = getBindsFromExps es
getBindsFromExp (Ite{..})   = getBindsFromExps [cnd, expThen, expElse]
getBindsFromExp (KV{..})    = getBindsFromExps (Var . fst <$> kvSubs) >> getBindsFromExps (snd <$> kvSubs)
getBindsFromExp (Var v)     = do
  bindingExists <- uses _2 (M.member v)
  when (not bindingExists) $ do
    n' <- use _1; _1 += 1
    _2 %= M.insert v (FQBind { bindId   = n'
                             , bindName = v
                             , bindType = if isTag v then FQ.FTC FQ.boolFTyCon else FQ.FInt
                             , bindRef  = FQ.prop True
                             })
getBindsFromExp (UFCheck{..})    =
  getBindsFromExps $ uncurry (++) $ unzip ufArgs
getBindsFromExp (Number _)       = return ()
getBindsFromExp (Boolean _)      = return ()

getBindsFromExps :: (Traversable t) => t Expr -> S ()
getBindsFromExps = sequence_ . (fmap getBindsFromExp)
