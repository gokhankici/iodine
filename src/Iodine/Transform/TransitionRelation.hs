{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Transform.TransitionRelation
  (
    transitionRelation
  )
where

import           Iodine.Language.IR
import           Iodine.Transform.Horn
import           Iodine.Types
import           Iodine.Utils

import           Control.Lens
import           Data.Foldable
import qualified Data.HashSet as HS
import qualified Data.Sequence as SQ
import qualified Data.Text as T
import           Polysemy
import           Polysemy.State
import           Text.Printf
import           Text.Read (readEither)

type S = Stmt Int

transitionRelation :: S -> HornExpr
transitionRelation s =
  HAnd $
  transitionRelation' mempty LeftRun s |:>
  transitionRelation' mempty RightRun s

type PathCond = L (Expr Int)

transitionRelation' :: PathCond -> HornVarRun -> S -> HornExpr
transitionRelation' conds r stmt =
  case stmt of
    Block {..} ->
      HAnd $ transitionRelation' conds r <$> blockStmts

    Assignment {..} ->
      HAnd $
      HBinary HEquals (val assignmentLhs) (val assignmentRhs) |:>
      let result = HBinary HIff (tag assignmentLhs) (tagWithCond conds assignmentRhs)
      in case r of
           LeftRun ->
             dt_trace
             (printf "%s <- %s" (show $ tag assignmentLhs) (show $ tagWithCond conds assignmentRhs))
             result
           RightRun -> result

    IfStmt {..} ->
      let conds' = ifStmtCondition <| conds
          hc     = val ifStmtCondition
          c      = HBinary HEquals    hc (HInt 0)
          not_c  = HBinary HNotEquals hc (HInt 0)
          t      = transitionRelation' conds' r ifStmtThen
          e      = transitionRelation' conds' r ifStmtElse
      in  HOr $ HAnd (c |:> t) |:> HAnd (not_c |:> e)

    Skip {..} ->
      HBool True

 where
  ufVal :: Id -> HornAppReturnType -> L (Expr Int) -> HornExpr
  ufVal name t es = HApp name t hes
    where
      hes = fmap val $ keepVariables es

  mkUFName n = "uf_noname_" <> T.pack (show n)

  val :: Expr Int -> HornExpr
  val = \case
    Constant {..} -> parseVerilogInt constantValue
    Variable {..} -> HVar { hVarName   = varName
                          , hVarModule = varModuleName
                          , hVarIndex  = exprData
                          , hVarType   = Value
                          , hVarRun    = r
                          }
    UF {..}     -> ufVal ufName HornInt ufArgs
    IfExpr {..} -> ufVal name HornInt (ifExprCondition |:> ifExprThen |> ifExprElse)
      where name = mkUFName exprData
    Str {..}    -> notSupported
    Select {..} -> ufVal name HornInt (selectVar <| selectIndices)
      where name = mkUFName exprData

  tagWithCond :: PathCond -> Expr Int -> HornExpr
  tagWithCond es e =
    case es of
      SQ.Empty -> tag e
      _        -> ufTag (es |> e)

  ufTag :: L (Expr Int) -> HornExpr
  ufTag = HOr . fmap tag . keepVariables

  tag :: Expr Int -> HornExpr
  tag = \case
    Constant {..} -> HBool False
    Variable {..} -> HVar { hVarName   = varName
                          , hVarModule = varModuleName
                          , hVarIndex  = exprData
                          , hVarType   = Tag
                          , hVarRun    = r
                          }
    UF {..}     -> ufTag ufArgs
    IfExpr {..} -> ufTag (ifExprCondition |:> ifExprThen |> ifExprElse)
    Str {..}    -> HBool False
    Select {..} -> ufTag (selectVar <| selectIndices)

{- |
Given a list of expressions, this returns a list of variables that appear in the
expressions without any duplicates.
-}
keepVariables :: L (Expr Int) -> L (Expr Int)
keepVariables es = goEs es & evalState HS.empty & run
  where
    goE :: Member (State (HS.HashSet (Expr Int))) r
        => Expr Int
        -> Sem r (L (Expr Int))
    goE Constant{..}   = return mempty
    goE v@Variable{..} = do hasVar <- gets $ HS.member v
                            if hasVar
                              then return mempty
                              else do modify $ HS.insert v
                                      return $ SQ.singleton v
    goE UF{..}         = goEs ufArgs
    goE IfExpr{..}     = goEs $ ifExprCondition |:> ifExprThen |> ifExprElse
    goE Str{..}        = return mempty
    goE Select{..}     = goEs $ selectVar <| selectIndices

    goEs :: Member (State (HS.HashSet (Expr Int))) r
         => L (Expr Int)
         -> Sem r (L (Expr Int))
    goEs = fmap (foldl' (<>) mempty) . traverse goE


parseVerilogInt :: Id -> HornExpr
parseVerilogInt value = case readEither v' of
  Left  _ -> HConstant value
  Right n -> HInt n
 where
  v  = T.unpack value
  v' = case v of
    '0' : 'b' : rst -> rst
    _               -> v
