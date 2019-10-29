{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Iodine.Language.IR
  ( Expr (..)
  , AssignmentType (..)
  , Stmt (..)
  , Module (..)
  , Port (..)
  , Variable (..)
  , Event (..)
  , AlwaysBlock (..)
  -- , mapExpr, mapStmt
  )
where

import           Iodine.Language.Types
import qualified Data.HashMap.Strict as HM

import           GHC.Generics hiding (moduleName)

data Variable =
    Wire {variableName :: Id}
  | Register {variableName :: Id}
  deriving (Show)

data Port =
    Input  { portVariable :: Variable }
  | Output { portVariable :: Variable }
  deriving (Show)

data Expr a =
  Constant { constantValue :: Id
           , exprData  :: a
           }
  | Variable { varName       :: Id
             , varModuleName :: Id
             , exprData      :: a
             }
  | UF { ufName   :: Id
       , ufArgs   :: L (Expr a)
       , exprData :: a
       }
  | IfExpr { ifExprCondition :: Expr a
           , ifExprThen      :: Expr a
           , ifExprElse      :: Expr a
           , exprData        :: a
           }
  | Str { strValue :: Id
        , exprData  :: a
        }
  | Select { selectVar     :: Expr a
           , selectIndices :: L (Expr a)
           , exprData      :: a
           }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data AssignmentType = Blocking | NonBlocking | Continuous
                    deriving (Show, Generic)

data Stmt a =
  Block { blockStmts :: L (Stmt a)
        , stmtData   :: a
        }
  | Assignment { assignmentType :: AssignmentType
               , assignmentLhs  :: Expr a
               , assignmentRhs  :: Expr a
               , stmtData       :: a
               }
  | IfStmt   { ifStmtCondition :: Expr a
             , ifStmtThen      :: Stmt a
             , ifStmtElse      :: Stmt a
             , stmtData       :: a
             }
  | ModuleInstance { moduleInstanceType  :: Id
                   , moduleInstanceName  :: Id
                   , moduleInstancePorts :: HM.HashMap Id (Expr a)
                   , stmtData            :: a
                   }
  -- this is added after the SSA step
  | PhiNode { phiLhs :: Expr a
            , phiRhs :: L (Expr a)
            , stmtData :: a
            }
  | Skip { stmtData :: a }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data Event a =
  PosEdge { eventExpr :: Expr a
          , eventData :: a
          }
  | NegEdge { eventExpr :: Expr a
            , eventData :: a
            }
  | Star { eventData :: a }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data AlwaysBlock a =
  AlwaysBlock { abEvent :: Event a
              , abStmt  :: Stmt a
              , abData  :: a
              }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data Module a =
  Module { moduleName   :: Id
         , ports        :: L Port
         , variables    :: L Variable
         , gateStmts    :: L (Stmt a)
         , alwaysBlocks :: L (AlwaysBlock a)
         , moduleData   :: a
         }
  deriving (Show, Generic, Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------
-- Typeclass Instances
-- -----------------------------------------------------------------------------

-- -- MapExpr

-- class MapExpr t where
--   mapExpr :: (Expr a -> Expr a) -> t a -> t a

-- instance MapExpr Stmt where
--   mapExpr f Block{..} = Block { blockStmts = fmap (mapExpr f) blockStmts, .. }
--   mapExpr f Assignment{..} = Assignment { assignmentLhs = f assignmentLhs
--                                         , assignmentRhs = f assignmentRhs
--                                         , ..
--                                         }
--   mapExpr f IfStmt{..} = IfStmt { ifStmtCondition = f ifStmtCondition
--                                 , ifStmtThen = mapExpr f ifStmtThen
--                                 , ifStmtElse = mapExpr f ifStmtElse
--                                 , ..
--                                 }
--   mapExpr f ModuleInstance{..} = ModuleInstance{ moduleInstancePorts = HM.map f moduleInstancePorts
--                                                , ..
--                                                }
--   mapExpr _ Skip{..} = Skip{..}
--   mapExpr f PhiNode{..} = PhiNode { phiLhs = f phiLhs
--                                   , phiRhs = f <$> phiRhs
--                                   , ..
--                                   }

-- instance MapExpr Event where
--   mapExpr f PosEdge{..} = PosEdge { eventExpr = f eventExpr, .. }
--   mapExpr f NegEdge{..} = NegEdge { eventExpr = f eventExpr, .. }
--   mapExpr _ Star{..} = Star { .. }

-- instance MapExpr AlwaysBlock where
--   mapExpr f AlwaysBlock{..} = AlwaysBlock { abEvent = mapExpr f abEvent
--                                           , abStmt  = mapExpr f abStmt
--                                           , ..
--                                           }

-- instance MapExpr Module where
--   mapExpr f Module{..} = Module { gateStmts = fmap (mapExpr f) gateStmts
--                                 , alwaysBlocks = fmap (mapExpr f) alwaysBlocks
--                                 , ..
--                                 }

-- -- MapStmt

-- class MapExpr t => MapStmt t where
--   mapStmt :: (Stmt a -> Stmt a) -> t a -> t a

-- instance MapStmt AlwaysBlock where
--   mapStmt f AlwaysBlock{..} = AlwaysBlock { abStmt  = f abStmt
--                                           , ..
--                                           }

-- instance MapStmt Module where
--   mapStmt f Module{..} = Module { gateStmts = fmap f gateStmts
--                                 , alwaysBlocks = fmap (mapStmt f) alwaysBlocks
--                                 , ..
--                                 }
