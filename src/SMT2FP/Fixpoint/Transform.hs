{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module SMT2FP.Fixpoint.Transform where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as M
import           Text.Printf

import           SMT2FP.SMT.Types
import           SMT2FP.Fixpoint.Types

data St = St { _invs          :: M.HashMap Id FQInv
             , _ufs           :: M.HashMap Id FQUF
             , _binds         :: M.HashMap Id FQBind
             , _constraints   :: [FQConstraint]
             , _wfConstraints :: [FQWFConstraint]
             , _n             :: Int
             } 
makeLenses ''St
  
emptySt = St { _invs          = M.empty
             , _ufs           = M.empty
             , _binds         = M.empty
             , _constraints   = []
             , _wfConstraints = []
             , _n             = 0
             }

type S = State St

smt2fp :: [Command] -> St
smt2fp cs = evalState comp emptySt
  where
    runCmds s = sequence_ (s <$> cs)
    comp = do sequence_ $ runCmds <$> [ gather1
                                      , gather2
                                      , emitConstraints
                                      ]
              emitWFConstraints
              get

--------------------------------------------------------------------------------
gather1 :: Command -> S ()
--------------------------------------------------------------------------------
gather1 (DeclareFun{..}) =
  invs %= M.insert dfName (FQInv { invName  = dfName
                                 , invArity = length dfArgs
                                 })
gather1 (DeclareConst{..}) =
  ufs %= M.insert dcName (FQUF { ufName  = dcName
                               , ufArity = l dcArg
                               })
  where
    l (A ixs _) = length ixs
    l _         = throw $ PassError "uf type should be an array"
gather1 _ = return ()

--------------------------------------------------------------------------------
gather2 :: Command -> S ()
--------------------------------------------------------------------------------
gather2 (Assert t)   = gatherTerm t
gather2 _            = return ()  

gatherTerm              :: Term -> S ()
gatherTerm (Forall{..}) = gatherTerm term
gatherTerm (BinOp{..})  = gatherTerm termL >> gatherTerm termR
gatherTerm (Ands ts)    = sequence_ $ gatherTerm <$> ts
gatherTerm (App{..})    = sequence_ $ (gatherTerm . Var) <$> fArgs
gatherTerm (Select{..}) = sequence_ $ (gatherTerm . Var) <$> arrIndices
gatherTerm (Ite{..})    = sequence_ $ gatherTerm <$> [termC, termL, termR]
gatherTerm (Var v)      = do
  hasV <- uses binds (M.member v)
  isUF <- uses ufs (M.member v)
  when (not hasV && not isUF) $ do
    n' <- use n; n += 1
    binds %= M.insert v (FQBind { bindId   = n'
                                , bindName = v
                                , bindExpr = FQBoolean True
                                })
gatherTerm (Number _)   = return ()
gatherTerm (Boolean _)  = return ()

--------------------------------------------------------------------------------
emitConstraints :: Command -> S ()
--------------------------------------------------------------------------------
emitConstraints (Assert t) =
  case t of
    Forall{..} -> case term of
                    BinOp IMPLIES hd bd -> do
                      n' <- use n; n += 1
                      lhs <- emitT hd
                      rhs <- emitT bd
                      constraints %= (:) (FQConstraint { constraintId    = n'
                                                       , constraintBinds = [] -- TODO
                                                       , constraintLhs   = lhs
                                                       , constraintRhs   = rhs
                                                       })
                    _                   -> err
    _          -> err
  where
    err = throw $ PassError "assert term is not of the form forall x . (=> P Q)"
emitConstraints _          = return ()

emitT :: Term -> S FQExpr
emitT (BinOp{..})  = do
  let op = case binop of
             PLUS    -> FQPLUS
             EQU     -> FQEQU
             GE      -> FQGE
             IMPLIES -> FQIMPLIES
  lhs <- emitT termL
  rhs <- emitT termR
  return $ FQBinOp { fqExpOp = op
                   , fqExpL  = lhs
                   , fqExpR  = rhs
                   }
emitT (Select{..}) = do
  uf <- uses ufs (M.lookupDefault err arrName)
  return $ FQUFCall { callUF   = uf
                    , callArgs = arrIndices
                    }
  where
    err = throw $ PassError $ printf "cannot find uf %s" arrName
emitT (App{..})    = do
  inv <- uses invs (M.lookupDefault err fName)
  return $ FQInvCall { callInv  = inv
                     , callArgs = fArgs
                     }
  where
    err = throw $ PassError $ printf "cannot find inv %s" fName
emitT (Ands ts)    = mapM emitT ts >>= return . FQAnds
emitT (Ite{..})    = do
  cExpr <- emitT termC
  lExpr <- emitT termL
  rExpr <- emitT termR
  return $ FQAnds [ FQBinOp FQIMPLIES cExpr         lExpr
                  , FQBinOp FQIMPLIES (FQNot cExpr) rExpr
                  ]
  where
emitT (Var v)      = return $ FQVar v
emitT (Number n)   = return $ FQNumber n
emitT (Boolean b)  = return $ FQBoolean b
emitT t            = throw $ PassError $ printf "unsupported term in emitT: %s" (show t)

--------------------------------------------------------------------------------
emitWFConstraints :: S ()
--------------------------------------------------------------------------------
emitWFConstraints = do
  wfConstraints <~ uses invs ((map FQWFConstraint) . M.elems)

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------
invArgs :: FQInv -> [Id]
invArgs (FQInv{..}) = []
