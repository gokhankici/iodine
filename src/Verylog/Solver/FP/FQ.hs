{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Solver.FP.FQ ( toFqFormat
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
import qualified Data.IntMap.Strict         as IM
import           Text.Printf
import qualified Data.Foldable              as F
import qualified Data.Sequence              as SQ

import qualified Language.Fixpoint.Types    as FQT
import           Language.Fixpoint.Types    hiding (Expr(..), KV)

toFqFormat :: FPSt -> GInfo SubC HornId
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

      custom n (QualifImp l rs) = custom1 n (id2Str l) (F.toList $ id2Str <$> rs)
      custom n (QualifPairs vs) = custom2 n (F.toList $ id2Str <$> vs)
      custom n (QualifIff l rs) = custom3 n (id2Str l) (F.toList $ id2Str <$> rs)
      custom _ (QualifAssume _) = []

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

      custom3 n l rs =
        [ mkQual
          (symbol $ "Custom3_" ++ show n ++ "_" ++ show n2)
          ( [ QP (symbol "v") PatNone FInt
            , QP (symbol  l ) (PatExact (symbol $ prefix ++ "_" ++ l)) (FTC boolFTyCon)
            ] ++
            [ QP (symbol r) (PatExact (symbol $ prefix ++ "_" ++ r)) (FTC boolFTyCon)
            | r <- rs
            ]
          )
          (FQT.PIff (eVar l) $ FQT.POr [eVar v | v <- rs])
          (dummyPos "")
        | (n2, prefix) <- zip ([1..] :: [Int]) ["VLT", "VRT"]
        ]
  in  fi cns wfs binders gConsts dConsts cuts qualifiers bindMds highOrBinds highOrQuals assrts axiomEnv dataDecls 

makeConstraints :: FPSt -> [SubC HornId]
makeConstraints fpst = snd $ IM.foldl' gos (0, mempty) (fpst ^. fpConstraints)
  where
    gos :: (Integer, [SubC HornId]) -> SQ.Seq Inv -> (Integer, [SubC HornId])
    gos = F.foldl' go

    go :: (Integer, [SubC HornId]) -> Inv -> (Integer, [SubC HornId])
    go (n, subcs) horn = (n+1, mc (n, horn) : subcs)

    mc (n, Horn{..}) = helper hBody hHead n hId

    helper bdy' hd n hId =
      let bdy = Ands [eqs, bdy']
      in  mkSubC
          (env (bdy SQ.<| hd SQ.<| mempty))
          (RR FInt (Reft (symbol "v", convertExpr bdy)))
          (RR FInt (Reft (symbol "v", convertExpr hd)))
          (Just n)              -- id
          []                    -- tags
          hId                   -- metadata

    env es = insertsIBindEnv (F.toList $ getBindIds fpst es) emptyIBindEnv

    eqs = Ands [ BinOp IFF (Var x1t) (Var x2t)
               | q       <- fpst ^. fpQualifiers
               , (x1,x2) <- case q of
                              QualifAssume vs -> twoPairs vs
                              _               -> []
               , (x1t, x2t) <- zip
                               (F.toList $ makeBothTags $ SQ.singleton x1)
                               (F.toList $ makeBothTags $ SQ.singleton x2)
               ]


makeWFConstraints :: FPSt -> [WfC HornId]
makeWFConstraints fpst = concatMap mwf (fpst ^. fpABs)
  where
    mwf a@AB{..} =
      let allAs = seqNub $ makeInvArgs fmt a SQ.>< extraEnv fpst
          ids   = F.toList $ getBindIds fpst (Var <$> allAs)
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
    mkBE FQBind{..} = ( bindId
                      , FQT.symbol bindName
                      , RR bindType (reft (FQT.symbol "v") bindRef)
                      )

convertExpr :: Expr -> FQT.Expr
convertExpr BinOp{..} =
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
convertExpr Ite{..} = pIte c el er
  where
    c  = convertExpr cnd
    el = convertExpr expThen
    er = convertExpr expElse
convertExpr KV{..} = FQT.PKVar
                     (FQT.KV $ symbol (makeInv kvId))
                     (mkSubst $ F.foldr' f [] kvSubs)
  where
    f (v,e) acc = (symbol v, convertExpr e) : acc
convertExpr (Var v)       = eVar v
convertExpr UFCheck{..} =
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

getBindIds :: FPSt -> SQ.Seq Expr -> SQ.Seq Int
getBindIds fpst es = runReader (mapM getBindId ids) fpst
  where
    ids   = f2seq $ F.foldl' (\s e -> s `HS.union` getIds e ) HS.empty es

    getBindId   :: Id -> Reader FPSt Int
    getBindId v = views fpBinds (bindId . M.lookupDefault (errMsg v) v)

    errMsg v = throw $ PassError $ printf "cannot find %s in binders" v

    helper []      = seq2set $ extraEnv fpst
    helper (e:es') = L.foldl' (\s e' -> getIds e' `HS.union` s) (getIds e) es'

    getIds :: Expr -> HS.HashSet Id
    getIds BinOp{..}    = getIds expL `HS.union` getIds expR
    getIds (Ands es')   = helper es'
    getIds Ite{..}      = getIds cnd `HS.union` getIds expThen `HS.union` getIds expElse
    getIds KV{..}       = let f acc (v,e) = getIds e `HS.union` HS.insert v acc
                          in F.foldl' f mempty kvSubs
    getIds (Var v)      = HS.singleton v
    getIds UFCheck{..}  = 
      let (as1,as2) = unzip $ map (over both idFromExp) ufArgs
          (n1,n2)   = ufNames & both %~ idFromExp
      in HS.fromList $ n1:n2:as1 ++ as2
    getIds e            = HS.singleton $ getConstantName e

type UFAcc = (Int, [(Symbol, Sort)])

getUFGlobals :: FPSt -> SEnv Sort
getUFGlobals fpst = fromListSEnv $ snd $ L.foldl' goStmt (0, []) ((^.aStmt) <$> fpst^.fpABs)
  where
    goStmt :: UFAcc -> Stmt -> UFAcc
    goStmt acc s = let ves = stmtCollectVExpr s
                   in F.foldl' goExpr acc ves

    goExpr :: UFAcc -> VExpr -> UFAcc
    goExpr acc VVar{..} = acc
    goExpr (!n, !l) ve@VUF{..} =
      let args  = vexprPortSeq ve
          arity = F.length args
          s     = if   arity > 0
                  then mkFFunc n (replicate (arity+1) FInt)
                  else FInt
          g     = (symbol vFuncName, s)
          n'    = n + 1
          l'    = g : l
      in n' `seq` l' `seq` (n', l')

extraEnv :: FPSt -> SQ.Seq Id
extraEnv fpst = makeBothTags . f2seq $ s1 `HS.union` s2
  where
    s1      = F.foldl' h mempty (fpst ^. fpQualifiers)
    s2      = fpst ^. fpAnnotations ^. sources
    h acc q = F.foldl' (flip HS.insert) acc (qualifVars q)

