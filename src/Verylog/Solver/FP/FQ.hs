{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.FP.FQ ( toFqFormat ) where

import Verylog.Solver.Common
import Verylog.Solver.FP.Types
import Verylog.Language.Types

import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Set                   as S
import qualified Data.HashSet               as HS
import qualified Data.HashMap.Strict        as M
import           Text.Printf

import qualified Language.Fixpoint.Types    as FQ
import           Language.Fixpoint.Types    hiding (Expr(..))

-- import Debug.Trace

toFqFormat :: FPSt -> GInfo SubC ()
toFqFormat fpst =
  let cns         = makeConstraints   fpst
      wfs         = makeWFConstraints fpst
      binders     = makeBinders       (fpst ^. fpBinds)
      gConsts     = emptySEnv
      dConsts     = emptySEnv
      cuts        = KS HS.empty
      qualifiers  = [ mkQual
                      (symbol "Eq")
                      [(symbol "v", FInt), (symbol "x", FInt), (symbol "y", FInt)] 
                      (FQ.PAtom Eq (eVar "x") (eVar "y"))
                      (dummyPos "")
                    ]
      bindMds     = M.empty
      highOrBinds = False
      highOrQuals = False
      assrts      = []
      axiomEnv    = AEnv [] [] M.empty
      dataDecls   = []
  in  fi cns wfs binders gConsts dConsts cuts qualifiers bindMds highOrBinds highOrQuals assrts axiomEnv dataDecls 

makeConstraints :: FPSt -> [SubC ()]
makeConstraints fpst = mc <$> (fpst ^. fpConstraints)
  where
    mc (Inv{..})  = helper invBody Structure { propName   = makeInv invId
                                             , propArgs   = invArgs
                                             , propParams = invParams
                                             }
    mc (Prop{..}) = helper propL   propR
    env es        = insertsIBindEnv (getBindIds fpst es) emptyIBindEnv
    helper el er  = mkSubC
                    (env [el,er])
                    (RR FInt (Reft (symbol "v", convertExpr el)))
                    (RR FInt (Reft (symbol "v", convertExpr er)))
                    Nothing     -- id
                    []          -- tags
                    ()

makeWFConstraints :: FPSt -> [WfC ()]
makeWFConstraints fpst = concatMap mwf (fpst ^. fpInvs)
  where
    mwf i@(InvFun{..}) =
      let (arg1:args) = fst <$> argVars i
          ids = getBindIds fpst (Var <$> args)
      in wfC
         (insertsIBindEnv ids emptyIBindEnv)
         (RR FInt (Reft ( symbol arg1
                        , FQ.PKVar (KV $ symbol invFunName) (mkSubst [])
                        )))
         ()

makeBinders   :: M.HashMap Id FQBind -> FQ.BindEnv
makeBinders m = bindEnvFromList l
  where
    l                 = mkBE <$> M.elems m
    mkBE (FQBind{..}) = ( bindId
                        , FQ.symbol bindName
                        , (RR bindType (reft (FQ.symbol "v") bindRef))
                        )

convertExpr :: Expr -> FQ.Expr
convertExpr (BinOp{..}) =
  case bOp of
    EQU     -> FQ.PAtom Eq   el er
    LE      -> FQ.PAtom Le   el er
    GE      -> FQ.PAtom Ge   el er
    PLUS    -> FQ.EBin  Plus el er
    AND     -> pAnd [el, er]
    OR      -> pOr [el, er]
    IMPLIES -> FQ.PImp el er
  where
    el = convertExpr expL
    er = convertExpr expR
convertExpr (Ands es) = pAnd (convertExpr <$> es)
convertExpr (Ite{..}) = pIte c el er
  where
    c  = convertExpr cnd
    el = convertExpr expThen
    er = convertExpr expElse
convertExpr (Structure{..}) = FQ.PKVar (KV (symbol propName)) sub
  where
    syms = (symbol . fst) <$> argVars' propName propArgs
    es   = eVar <$> propArgs
    sub  = mkSubst (zip syms es)
convertExpr (Var v)       = eVar v
convertExpr (UFCheck{..}) = prop True
convertExpr (Number n)    = expr n
convertExpr (Boolean b)   = prop b

getBindIds :: FPSt -> [Expr] -> [Int]
getBindIds fpst es = runReader (mapM getBindId ids) fpst
  where
    ids   = S.toList idSet
    idSet = foldr (\e s -> s `S.union` getIds e ) S.empty es

    getBindId   :: Id -> Reader FPSt Int
    getBindId v = views fpBinds (bindId . (M.lookupDefault (err v) v))

    err v = throw $ PassError $ printf "cannot find %s in binders" v

    helper []     = S.empty
    helper (e:es) = foldr (\e s -> getIds e `S.union` s) (getIds e) es

    getIds :: Expr -> S.Set Id
    getIds (BinOp{..})      = helper [expL, expR]
    getIds (Ands es)        = helper es
    getIds (Ite{..})        = helper [cnd, expThen, expElse]
    getIds (Structure{..}) = S.fromList (propArgs ++ args)
      where
        args = tail $ fst <$> argVars' propName propArgs
    getIds (Var v)          = S.singleton v
    getIds (UFCheck{..})    = 
      let (as1,as2) = unzip $ map (over both idFromExp) ufArgs
          (n1,n2)   = ufNames & both %~ idFromExp
      in S.fromList $ ufFunc:n1:n2:as1 ++ as2
    getIds (Number _)       = S.empty
    getIds (Boolean _)      = S.empty

