{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.FP.FQ ( toFqFormat
                            , Metadata
                            , convertExpr
                            ) where

import Verylog.Solver.Common
import Verylog.Solver.FP.Types
import Verylog.Language.Types
import Verylog.Transform.Utils

import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import qualified Data.List                  as L
import qualified Data.HashSet               as HS
import qualified Data.HashMap.Strict        as M
import           Text.Printf

import qualified Language.Fixpoint.Types    as FQT
import           Language.Fixpoint.Types    hiding (Expr(..), KV)

-- import Debug.Trace  

type Metadata = HornId

toFqFormat :: FPSt -> GInfo SubC Metadata
toFqFormat fpst =
  let cns         = makeConstraints   fpst
      wfs         = makeWFConstraints fpst
      binders     = makeBinders       (fpst ^. fpBinds)
      gConsts     = getUFGlobals fpst
      dConsts     = emptySEnv
      cuts        = KS HS.empty
      qualifiers  = qualifiersWithPath -- ++ qualifiersNoPat
      qualifiersWithPath  =
        [ mkQual
          (symbol "Eq1")
          [ QP (symbol "v") PatNone FInt
          , QP (symbol "x") (PatPrefix (symbol "VL_") 1) FInt
          , QP (symbol "y") (PatPrefix (symbol "VR_") 1) FInt
          ] 
          (FQT.PAtom Eq (eVar "x") (eVar "y"))
          (dummyPos "")
        , mkQual
          (symbol "Eq2")
          [ QP (symbol "v") PatNone FInt
          , QP (symbol "x") (PatPrefix (symbol "VLT") 1) (FTC boolFTyCon)
          , QP (symbol "y") (PatPrefix (symbol "VRT") 1) (FTC boolFTyCon)
          ] 
          (FQT.PIff (eVar "x") (eVar "y"))
          (dummyPos "")
        ]
        ++
        [ mkQual
          (symbol (printf "Zero%d" (n::Int) :: String))
          [ QP (symbol "v") PatNone FInt
          , QP (symbol "x") (PatPrefix (symbol pre_x) 1) (FTC boolFTyCon)
          ] 
          (FQT.PIff (eVar "x") FQT.PFalse)
          (dummyPos "")
        | (n,pre_x) <-
            zip [1..]
            [ "VLT_"  , "VRT_" ]
        ]
        ++
        concat [ custom n q
               | (n, q) <- zip ([1..] :: [Int]) (fpst ^. fpQualifiers)
               ]
      bindMds     = M.empty
      highOrBinds = False
      highOrQuals = False
      assrts      = []
      axiomEnv    = AEnv [] [] M.empty
      dataDecls   = []

      custom n (QualifImpl l rs) = custom1 n l rs
      custom n (QualifEqs vs)    = custom2 n vs

      custom1 n l rs = 
        [ mkQual
          (symbol $ "Custom1_" ++ show n ++ "_" ++ show n2)
          ( [ QP (symbol "v") PatNone FInt
            , QP (symbol  l ) (PatExact (symbol $ prefix ++ "_" ++ l)) (FTC boolFTyCon)
            ] ++
            [ QP (symbol r) (PatExact (symbol $ prefix ++ "_" ++ r)) (FTC boolFTyCon)
            | r <- rs
            ]
          )
          (FQT.PImp (eVar l) $ FQT.POr [eVar v | v <- rs])
          (dummyPos "")
        | (n2, prefix) <- zip ([1..] :: [Int]) ["VLT", "VRT"]
        ]

      custom2 n vs =
        [ mkQual
          (symbol $ "Custom2_" ++ show n ++ "_" ++ show n2)
          [ QP (symbol "v") PatNone FInt
          , QP (symbol x1) (PatExact (symbol $ prefix ++ x1)) (FTC boolFTyCon)
          , QP (symbol x2) (PatExact (symbol $ prefix ++ x2)) (FTC boolFTyCon)
          ]
          (FQT.PImp (eVar x1) (eVar x2))
          (dummyPos "")
        | (n2',(x1',x2')) <- zip ([1,3..] :: [Int]) (twoPairs vs)
        , (n2, (x1,x2))   <- [(n2',(x1',x2')), (n2' + 1, (x2',x1'))]
        , prefix          <- ["VLT_", "VRT_"]
        ]
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
      let allAs = HS.toList . HS.fromList $ makeInvArgs fmt a ++ extraEnv fpst
          ids   = getBindIds fpst (Var <$> allAs)
          i     = a ^. aId
      in wfC
         (insertsIBindEnv ids emptyIBindEnv)
         (RR FInt (Reft ( symbol "v"
                        , FQT.PKVar (FQT.KV $ symbol (makeInvPred a)) (mkSubst [])
                        )))
         (HornId i (InvWF i))


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
    IFF     -> FQT.PIff el er
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
convertExpr e = FQT.EVar $ FQT.symbol $ getConstantName e

getBindIds :: FPSt -> [Expr] -> [Int]
getBindIds fpst es = runReader (mapM getBindId ids) fpst
  where
    ids   = HS.toList idSet
    idSet = foldr (\e s -> s `HS.union` getIds e ) HS.empty es

    getBindId   :: Id -> Reader FPSt Int
    getBindId v = views fpBinds (bindId . (M.lookupDefault (errMsg v) v))

    errMsg v = throw $ PassError $ printf "cannot find %s in binders" v

    helper []      = HS.fromList $ extraEnv fpst
    helper (e:es') = L.foldl' (\s e' -> getIds e' `HS.union` s) (getIds e) es'

    getIds :: Expr -> HS.HashSet Id
    getIds (BinOp{..})      = getIds expL `HS.union` getIds expR
    getIds (Ands es')       = helper es'
    getIds (Ite{..})        = getIds cnd `HS.union` getIds expThen `HS.union` getIds expElse
    getIds (KV{..})         = let (vs,es') = unzip kvSubs
                              in HS.fromList vs `HS.union` helper es'
    getIds (Var v)          = HS.singleton v
    getIds (UFCheck{..})    = 
      let (as1,as2) = unzip $ map (over both idFromExp) ufArgs
          (n1,n2)   = ufNames & both %~ idFromExp
      in HS.fromList $ n1:n2:as1 ++ as2
    getIds e                = HS.singleton $ getConstantName e


getUFGlobals :: FPSt -> SEnv Sort
getUFGlobals fpst = fromListSEnv $ snd $ M.foldrWithKey mkGlobF (0, []) (fpst^.fpUFs)
  where
    mkGlobF f args (n,l) =
      let arity = length args
          s     = if   arity > 0
                  then mkFFunc n (replicate (arity+1) FInt)
                  else FInt
          g     = (symbol f, s)
      in (n+1, g:l)
    

extraEnv :: FPSt -> [Id]
extraEnv fpst = makeBothTags . s $ concatMap qualifVars (fpst ^. fpQualifiers) ++ (fpst ^. fpSources)
  where
    s = HS.toList . HS.fromList
