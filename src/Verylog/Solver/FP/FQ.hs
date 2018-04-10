{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.FP.FQ ( toFqFormat ) where

import Verylog.Solver.Common
import Verylog.Solver.FP.Types
import Verylog.Language.Types
import Verylog.Transform.Utils

import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
-- import qualified Data.List                  as L
import qualified Data.Set                   as S
import qualified Data.HashSet               as HS
import qualified Data.HashMap.Strict        as M
import           Text.Printf

import qualified Language.Fixpoint.Types    as FQT
import           Language.Fixpoint.Types    hiding (Expr(..), KV)

import qualified Text.PrettyPrint.HughesPJ as PP

import Debug.Trace  

type Metadata = HornId

instance Loc HornId where
  srcSpan _ = dummySpan

instance Fixpoint HornId where
  toFix (SingleBlock n)           = PP.text "always block id:"
                                    PP.<+> PP.int n
  toFix (InterferenceBlock n1 n2) = PP.text "interference of always blocks: "
                                    PP.<+> PP.parens (PP.int n1 PP.<> PP.comma PP.<+> PP.int n2)

toFqFormat :: FPSt -> GInfo SubC Metadata
toFqFormat fpst =
  let cns         = makeConstraints   fpst
      wfs         = makeWFConstraints fpst
      binders     = makeBinders       (fpst ^. fpBinds)
      gConsts     = getUFGlobals fpst
      dConsts     = emptySEnv
      cuts        = KS HS.empty
      -- qualifiers  = traceFix "qualifiers" qualifiers'
      qualifiers  = [ mkQual
                      (symbol (printf "Eq%d" (n::Int) :: String))
                      [ QP (symbol "v") PatNone FInt
                      , QP (symbol "x") (PatPrefix (symbol pre_x) 1) FInt
                      , QP (symbol "y") (PatPrefix (symbol pre_y) 1) FInt
                      ] 
                      (FQT.PAtom Eq (eVar "x") (eVar "y"))
                      (dummyPos "")
                    | (n,(pre_x,pre_y)) <- zip [1..]
                                           [ ("VL_"   , "VR_")
                                           , ("VLP_"  , "VRP_")
                                           , ("VLT_"  , "VRT_")
                                           , ("VLTP_" , "VRTP_")
                                           ]
                    ]
      bindMds     = M.empty
      highOrBinds = False
      highOrQuals = False
      assrts      = []
      axiomEnv    = AEnv [] [] M.empty
      dataDecls   = []
  in  fi cns wfs binders gConsts dConsts cuts qualifiers bindMds highOrBinds highOrQuals assrts axiomEnv dataDecls 

makeConstraints :: FPSt -> [SubC Metadata]
makeConstraints fpst = mc <$> zip [0..] (fpst ^. fpConstraints)
  where
    mc (n, (Horn{..})) = helper hBody hHead n hId
    env es        = insertsIBindEnv (getBindIds fpst es) emptyIBindEnv
    helper bdy hd n hId =
      let x = mkSubC
              (env [bdy,hd])
              (RR FInt (Reft (symbol "v", convertExpr bdy)))
              (RR FInt (Reft (symbol "v", convertExpr hd)))
              (Just n)          -- id
              []                -- tags
              hId               -- metadata
      in x -- trace (show x) x

makeWFConstraints :: FPSt -> [WfC Metadata]
makeWFConstraints fpst = concatMap mwf (fpst ^. fpABs)
  where
    mwf a@(AB{..}) =
      let allAs = makeInvArgs fmt a
                  ++ makeInvArgs fmt{primedVar=True} a
          ids = getBindIds fpst (Var <$> allAs)
      in wfC
         (insertsIBindEnv ids emptyIBindEnv)
         (RR FInt (Reft ( symbol "v"
                        , FQT.PKVar (FQT.KV $ symbol (makeInvPred a)) (mkSubst [])
                        )))
         (SingleBlock $ a ^. aId)

makeBinders   :: M.HashMap Id FQBind -> FQT.BindEnv
makeBinders m = bindEnvFromList l
  where
    l                 = mkBE <$> M.elems m
    mkBE (FQBind{..}) = ( bindId
                        , FQT.symbol bindName
                        , (RR bindType (reft (FQT.symbol "v") bindRef))
                        )

convertExpr :: Expr -> FQT.Expr
convertExpr (BinOp{..}) =
  case bOp of
    EQU     -> FQT.PAtom Eq   el er
    LE      -> FQT.PAtom Le   el er
    GE      -> FQT.PAtom Ge   el er
    PLUS    -> FQT.EBin  Plus el er
    AND     -> pAnd [el, er]
    OR      -> pOr [el, er]
    IMPLIES -> FQT.PImp el er
  where
    el = convertExpr expL
    er = convertExpr expR
convertExpr (Ands es) = pAnd (convertExpr <$> es)
convertExpr (Ite{..}) = pIte c el er
  where
    c  = convertExpr cnd
    el = convertExpr expThen
    er = convertExpr expElse
convertExpr (KV{..})      = FQT.PKVar
                            (FQT.KV $ symbol (makeInv kvId))
                            (mkSubst $ f <$> kvSubs)
  where
    f (v,e) = (symbol v, convertExpr e)
convertExpr (Var v)       = eVar v
convertExpr (UFCheck{..}) =
  let (largs, rargs) = unzip ufArgs
      (l, r)         = ufNames
      f              = dummyLoc $ symbol ufFunc
      mkVar          = eVar . idFromExp
      lSel           = mkEApp f (mkVar <$> largs)
      rSel           = mkEApp f (mkVar <$> rargs)
  in  pAnd [ FQT.PAtom Eq (mkVar l) lSel
           , FQT.PAtom Eq (mkVar r) rSel
           ]
convertExpr (Number n)    = FQT.EVar $ FQT.symbol $ getConstantName n
convertExpr (Boolean b)   = prop b

getBindIds :: FPSt -> [Expr] -> [Int]
getBindIds fpst es = runReader (mapM getBindId ids) fpst
  where
    ids   = S.toList idSet
    idSet = foldr (\e s -> s `S.union` getIds e ) S.empty es

    getBindId   :: Id -> Reader FPSt Int
    getBindId v = views fpBinds (bindId . (M.lookupDefault (errMsg v) v))

    errMsg v = throw $ PassError $ printf "cannot find %s in binders" v

    helper []      = S.empty
    helper (e:es') = foldr (\e' s -> getIds e' `S.union` s) (getIds e) es'

    getIds :: Expr -> S.Set Id
    getIds (BinOp{..})      = helper [expL, expR]
    getIds (Ands es')       = helper es'
    getIds (Ite{..})        = helper [cnd, expThen, expElse]
    getIds (KV{..})         = let (vs,es') = unzip kvSubs
                              in S.fromList vs `S.union` helper es'
    getIds (Var v)          = S.singleton v
    getIds (UFCheck{..})    = 
      let (as1,as2) = unzip $ map (over both idFromExp) ufArgs
          (n1,n2)   = ufNames & both %~ idFromExp
      in S.fromList $ n1:n2:as1 ++ as2
    getIds (Number n)       = S.singleton $ getConstantName n
    getIds (Boolean _)      = S.empty


getUFGlobals :: FPSt -> SEnv Sort
getUFGlobals fpst = fromListSEnv $ snd $ M.foldrWithKey mkGlobF (0, []) (fpst^.fpUFs)
  where
    mkGlobF f arity (n,l) =
      let s = if   arity > 0
              then mkFFunc n (replicate (arity+1) FInt)
              else FInt
          g = (symbol f, s)
      in (n+1, g:l)
    

