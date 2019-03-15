{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Transform.TransitionRelation ( next
                                            ) where

import           Control.Exception
import           Control.Monad.State.Lazy
import           Control.Lens
import qualified Data.HashMap.Strict      as M
import qualified Data.HashSet             as S
import           Data.List
import           Data.Foldable
import qualified Data.Sequence as SQ

import           Verylog.Transform.Utils hiding (fmt)
import           Verylog.Language.Types
import           Verylog.Solver.Common


type AsgnQueue = M.HashMap Id Int

data TRSt = TRSt { _trSt    :: St           -- state of the always block given to the function `next`
                 , _trFmt   :: VarFormat    -- store the format variable given to the function `next`
                 , _trAs    :: AsgnQueue    -- assignment counts
                 , _trBAs   :: AsgnQueue    -- assignment counts (blocking assignments only)
                 , _ifConds :: S.HashSet Id -- conditions of the if expressions in the stmt hierarchy
                 , _errs    :: [PassError]
                 }

makeLenses ''TRSt

type S = State TRSt

data Asgn = BA | NBA

type Updates = SQ.Seq (Id, Expr)

--------------------------------------------------------------------------------
next :: VarFormat -> AlwaysBlock -> (Expr,Updates)
--------------------------------------------------------------------------------
next fmt a = evalState comp initSt
  where
    initSt = TRSt { _trSt    = a^.aSt
                  , _trFmt   = fmt
                  , _trAs    = M.empty
                  , _trBAs   = M.empty
                  , _ifConds = S.empty
                  , _errs    = []
                  }
    comp = do
      es1 <- nextStmt (a^.aStmt)

      ps  <- uses (trSt . ports) (fmap varName) -- . filter isRegister)
      as  <- use trAs

      -- -- set the primed vars for the lhs of the assignments
      let up1 =
            M.foldlWithKey'
            (\acc v n ->
               if   v `elem` ps
               then (,)
                    (makeVarName fmt{taggedVar=True} v)
                    (makeVar     fmt{taggedVar=True, varId=Just n} v)
                    SQ.<|
                    (,)
                    (makeVarName fmt v)
                    (makeVar     fmt{varId=Just n} v)
                    SQ.<|
                    acc
               else acc)
            SQ.empty
            as                  -- blocking assignments

      -- set the primed vars for the untouched variables
      let up2 =
            foldl'
            (\acc v ->
               if not $ M.member v as -- not updated variables
               then (,)
                    (makeVarName fmt{taggedVar=True} v)
                    (makeVar     fmt{taggedVar=True} v) -- vt' = vt
                    SQ.<|
                    (,)
                    (makeVarName fmt v)
                    (makeVar     fmt v) -- v' = v
                    SQ.<|
                    acc
               else acc
            )
            SQ.empty
            ps

      errors <- use errs
      case errors of
        []  -> return (Ands (toList es1), up1 SQ.>< up2)
        e:_ -> throw e

--------------------------------------------------------------------------------
nextStmt :: Stmt -> S (SQ.Seq Expr)
--------------------------------------------------------------------------------
nextStmt (Block{..})           = foldl' (SQ.><) mempty <$> sequence (nextStmt <$> blockStmts)
nextStmt (BlockingAsgn{..})    = nextAsgn BA  lhs rhs
nextStmt (NonBlockingAsgn{..}) = nextAsgn NBA lhs rhs
nextStmt Skip                  = return mempty

-- ---------------------------------------------------------------------------
-- ASSUMPTION(S):
-- 1. the types of assignments to a variable is the same in both branches
-- 2. at most one assignment to a single variable
-- ---------------------------------------------------------------------------
nextStmt (IfStmt{..}) = do
  fmt <- use trFmt
  n <- getLastVarRHS fmt (vVarName ifCond)
  let condTrue  = BinOp GE n (Number 1)

  -- if condition is the value of an uninterpreted function,
  -- add a check that equates left & right runs for the value
  condUFCheck <- if rightVar fmt
                 then uf_eq ifCond
                 else return mempty

  -- store old if conds, and calculate the new one
  oldIfConds <- use ifConds
  let cvs = ifCondVars ifCond
  ifConds %= S.union cvs

  -- store the current state before running either branch
  oldSt <- getSt

  -- run the then branch,
  thenClauses   <- nextStmt thenStmt
  (thAs,thBAs)  <- getSt

  -- set back the old state
  setSt oldSt

  -- run the else branch,
  elseClauses   <- nextStmt elseStmt
  (elAs, elBAs) <- getSt

  -- calculate & set the new state
  let newAs  = M.unionWith max thAs  elAs
      newBAs = M.unionWith max thBAs elBAs

  setSt (newAs, newBAs)
  ifConds .= oldIfConds

  let thDiff = branchDif thAs elAs
      elDiff = branchDif elAs thAs

  let th  = Ands . toList $ thenClauses SQ.>< phiNodes fmt thAs thDiff
      el  = Ands . toList $ elseClauses SQ.>< phiNodes fmt elAs elDiff
      ite = Ite condTrue th el

  return $ condUFCheck SQ.|> ite

  where
    -- returns the variables changed in one branch but not in other
    branchDif myQ otherQ =
      M.differenceWith
      (\otherN myN ->
         if myN < otherN   -- if I don't have the latest number
         then Just otherN  -- add another equality to match that
         else Nothing      -- otherwise, skip
      ) otherQ myQ

    getSt :: S (AsgnQueue, AsgnQueue)
    getSt = (,) <$> use trAs <*> use trBAs

    setSt :: (AsgnQueue, AsgnQueue) -> S ()
    setSt (as,bas) = trAs .= as >> trBAs .= bas

    phiNodes :: VarFormat -> AsgnQueue -> AsgnQueue -> SQ.Seq Expr
    phiNodes fmt q qDiff =
      let fmtT = fmt{taggedVar=True}
      in  M.foldlWithKey'
          (\acc v n ->
             BinOp EQU
             (makeVar fmt{varId=Just n} v)
             (makeVar fmt{varId=M.lookup v q} v)
             SQ.<|
             BinOp EQU
             (makeVar fmtT{varId=Just n} v)
             (makeVar fmtT{varId=M.lookup v q} v)
             SQ.<|
            acc
          ) mempty qDiff

    ifCondVars = seq2set . varDeps

--------------------------------------------------------------------------------
nextAsgn :: Asgn -> Id -> VExpr -> S (SQ.Seq Expr)
--------------------------------------------------------------------------------
nextAsgn a l r = do es1 <- uf_eq r
                    es2 <- asgn
                    return $ es1 SQ.>< es2
  where
    ----------------------------------------
    asgn :: S (SQ.Seq Expr)
    ----------------------------------------
    asgn = do fmt <- use trFmt

              rhs   <- getLastVarRHS fmt (vVarName r)
              t_rhs <- tagRhs

              -- use a fresh var for the assignment
              case a of
                BA  -> incrBA  l
                NBA -> incrNBA l

              lhs   <- getLastVarLHS fmt            l
              t_lhs <- getLastVarLHS (mkTagged fmt) l

              return $
                BinOp EQU t_lhs t_rhs   -- update the tag   of lhs
                SQ.<| BinOp EQU lhs rhs -- update the value of lhs
                SQ.<| mempty

    ----------------------------------------
    tagRhs :: S Expr
    ----------------------------------------
    tagRhs = do
      fmt' <- uses trFmt mkTagged
      ts1  <- case r of
                VUF{..}  -> ufTagRhs
                VVar{..} -> getLastVarRHS fmt' vVarName
      ts2 <- (uses ifConds S.toList) >>= mapM (getLastVarRHS fmt')
      case ts2 of
        [] -> return ts1
        _  -> case nub $ plusVars (ts1:ts2) of -- add each unique operand only once
                []        -> error "TransitionRelation.tagRhs : this is weird"
                (es1:ess) -> return $ foldr (BinOp OR) es1 ess

    ----------------------------------------
    plusVars :: [Expr] -> [Expr]
    ----------------------------------------
    plusVars es = concatMap f es
      where
        f e = case e of
                BinOp{..} -> concatMap f [expL, expR]
                _         -> [e]

    ----------------------------------------
    ufTagRhs :: S Expr
    ----------------------------------------
    ufTagRhs = do
      fmt <- uses trFmt mkTagged
      vs <- ufAtomsRHS fmt r
      case SQ.viewl vs of
        SQ.EmptyL    -> return $ Boolean False -- rhs is constant, no tag propagation needed
        v SQ.:< rest -> return $ foldr (BinOp OR) v rest

    mkTagged fmt = fmt{taggedVar=True}


-- | Recursively look into the given variables arguments.
-- The returned list only contain ports.
varDeps :: VExpr -> SQ.Seq Id
varDeps (VVar{..}) = SQ.singleton vVarName
varDeps (VUF{..})  = foldl' (\acc v -> acc SQ.>< varDeps v) mempty vFuncArgs

-- | Arguments of the uf formatted with fmt
ufAtomsRHS :: VarFormat -> VExpr -> S (SQ.Seq Expr)
ufAtomsRHS _   v@(VVar{..}) = errs %= (:) (PassError $ "ufAtomsRHS called with a variable " ++ show v)
                              >> return mempty
ufAtomsRHS fmt uf@(VUF{..}) = sequence $ getLastVarRHS fmt <$> varDeps uf

-- | Return a UFCheck expression
uf_eq :: VExpr -> S (SQ.Seq Expr)
uf_eq (VVar{..}) = return mempty
uf_eq uf@(VUF{..}) = do
  fmt <- use trFmt
  let fmtL = fmt{leftVar=True,  rightVar=False}
      fmtR = fmt{leftVar=False, rightVar=True}
  varsL <- ufAtomsRHS fmtL uf
  varsR <- ufAtomsRHS fmtR uf
  let lhs = SQ.zip varsL varsR
      rhs = (makeVar fmtL vVarName, makeVar fmtR vVarName)
  return $ return
    UFCheck { ufFunc  = vFuncName
            , ufArgs  = toList lhs
            , ufNames = rhs
            }

-- --------------------------------------------------------------------------------
-- -- helper functions
-- --------------------------------------------------------------------------------

-- | Returned the last blocking assignment to the variable
getLastVarRHS :: VarFormat -> Id -> S Expr
getLastVarRHS fmt v = do mi <- uses trBAs (M.lookup v)
                         return $ makeVar fmt{varId=mi} v

-- | Returned the last assignment (blocking or non-blocking) to the variable
getLastVarLHS       :: VarFormat -> Id -> S Expr
getLastVarLHS fmt v = do mi <- uses trAs (M.lookup v)
                         return $ makeVar fmt{varId=mi} v

-- | Increment the blocking assignment index of the variable
incrBA :: Id -> S ()
incrBA v = do trAs  <~ uses trAs  (M.alter updateInd v)
              trBAs <~ uses trBAs (M.alter updateInd v)

-- | Increment the non-blocking assignment index of the variable
incrNBA :: Id -> S ()
incrNBA v = trAs  <~ uses trAs  (M.alter updateInd v)

-- | Increment the Maybe integer
-- Nothing -> Just 1
-- Just n  -> Just (n + 1)
updateInd         :: Maybe Int -> Maybe Int
updateInd Nothing = Just 1
updateInd m       = (+ 1) <$> m
