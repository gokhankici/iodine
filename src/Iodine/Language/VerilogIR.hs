{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Iodine.Language.VerilogIR
  ( Expr (..)
  , AssignmentType (..)
  , Stmt (..)
  , Module (..)
  , Port (..)
  , Variable (..)
  , Event (..)
  , AlwaysBlock (..)
  , mapExpr, mapStmt
  )
where

import           Iodine.Language.Types
import qualified Data.HashMap.Strict as HM

import           GHC.Generics hiding (moduleName)

data Variable a =
  Wire { variableName :: Id
       , variableData :: a
       }
  | Register {variableName :: Id
             , variableData :: a
             }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data Port a =
  Input  { portVariable :: Variable a
         , portData :: a
         }
  | Output { portVariable :: Variable a
           , portData :: a
           }
  deriving (Show, Generic, Functor, Foldable, Traversable)

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

data Stmt expr a =
  Block { blockStmts :: L (Stmt expr a)
        , stmtData   :: a
        }
  | Assignment { assignmentType :: AssignmentType
               , assignmentLhs  :: expr a
               , assignmentRhs  :: expr a
               , stmtData       :: a
               }
  | IfStmt   { ifStmtCondition :: expr a
             , ifStmtThen      :: Stmt expr a
             , ifStmtElse      :: Stmt expr a
             , stmtData       :: a
             }
  | ModuleInstance { moduleInstanceType  :: Id
                   , moduleInstanceName  :: Id
                   , moduleInstancePorts :: HM.HashMap Id (expr a)
                   , stmtData            :: a
                   }
  | Skip { stmtData :: a }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data Event expr a =
  PosEdge { eventExpr :: expr a
          , eventData :: a
          }
  | NegEdge { eventExpr :: expr a
            , eventData :: a
            }
  | Star { eventData :: a }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data AlwaysBlock stmt expr a =
  AlwaysBlock { abEvent :: Event expr a
              , abStmt  :: stmt expr a
              , abData  :: a
              }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data Module stmt expr a =
  Module { moduleName   :: Id
         , ports        :: L (Port a)
         , variables    :: L (Variable a)
         , gateStmts    :: L (stmt expr a)
         , alwaysBlocks :: L (AlwaysBlock stmt expr a)
         , moduleData   :: a
         }
  deriving (Show, Generic, Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------
-- Typeclass Instances
-- -----------------------------------------------------------------------------

-- MapExpr

class MapExpr t where
  mapExpr :: (e a -> e' a) -> t e a -> t e' a

instance MapExpr Stmt where
  mapExpr f Block{..} = Block { blockStmts = fmap (mapExpr f) blockStmts, .. }
  mapExpr f Assignment{..} = Assignment { assignmentLhs = f assignmentLhs
                                        , assignmentRhs = f assignmentRhs
                                        , ..
                                        }
  mapExpr f IfStmt{..} = IfStmt { ifStmtCondition = f ifStmtCondition
                                , ifStmtThen = mapExpr f ifStmtThen
                                , ifStmtElse = mapExpr f ifStmtElse
                                , ..
                                }
  mapExpr f ModuleInstance{..} = ModuleInstance{ moduleInstancePorts = HM.map f moduleInstancePorts
                                               , ..
                                               }
  mapExpr _ Skip{..} = Skip{..}

instance MapExpr Event where
  mapExpr f PosEdge{..} = PosEdge { eventExpr = f eventExpr, .. }
  mapExpr f NegEdge{..} = NegEdge { eventExpr = f eventExpr, .. }
  mapExpr _ Star{..} = Star { .. }

instance MapExpr stmt => MapExpr (AlwaysBlock stmt) where
  mapExpr f AlwaysBlock{..} = AlwaysBlock { abEvent = mapExpr f abEvent
                                          , abStmt  = mapExpr f abStmt
                                          , ..
                                          }

instance MapExpr stmt => MapExpr (Module stmt) where
  mapExpr f Module{..} = Module { gateStmts = fmap (mapExpr f) gateStmts
                                , alwaysBlocks = fmap (mapExpr f) alwaysBlocks
                                , ..
                                }

-- MapStmt

class MapStmt (t :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)  where
  mapStmt :: (s e a -> s' e a) -> t s e a -> t s' e a

instance MapStmt AlwaysBlock where
  mapStmt f AlwaysBlock{..} = AlwaysBlock { abStmt  = f abStmt
                                          , ..
                                          }

instance MapStmt Module where
  mapStmt f Module{..} = Module { gateStmts = fmap f gateStmts
                                , alwaysBlocks = fmap (mapStmt f) alwaysBlocks
                                , ..
                                }
