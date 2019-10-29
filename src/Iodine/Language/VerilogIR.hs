{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Iodine.Language.VerilogIR ( Expr (..)
                                 , AssignmentType (..)
                                 , Stmt (..)
                                 , Module (..)
                                 , Port (..)
                                 , Variable (..)
                                 , Event (..)
                                 , AlwaysBlock (..)
                                 ) where

import           Iodine.Language.Types (Id)

import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence       as SQ

type L = SQ.Seq

data Expr a =
  Constant { exprValue :: Id
           , exprData  :: a
           }
  | Variable { exprValue     :: Id
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
  | Str { exprValue :: Id
        , exprData  :: a
        }
  | Select { selectVar     :: Expr a
           , selectIndices :: L (Expr a)
           , exprData      :: a
           }
  deriving (Show)

data AssignmentType = Blocking | NonBlocking | Continuous
                    deriving (Show)

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
  deriving (Show)

data Module stmt expr a =
  Module { moduleName   :: Id
         , ports        :: L (Port a)
         , variables    :: L (Variable a)
         , gateStmts    :: L (stmt expr a)
         , alwaysBlocks :: L (AlwaysBlock stmt expr a)
         , moduleData   :: a
         }
  deriving (Show)

data Port a = Input  (Variable a) a
            | Output (Variable a) a
            deriving (Show)

data Variable a = Wire     Id a
                | Register Id a
                deriving (Show)

data Event expr a =
  PosEdge (expr a) a
  | NegEdge (expr a) a
  | Star a
  deriving (Show)

data AlwaysBlock stmt expr a =
  AlwaysBlock { abEvent :: Event expr a
              , abStmt  :: stmt expr a
              , abData  :: a
              }
  deriving (Show)

-- -----------------------------------------------------------------------------
-- Typeclass Instances
-- -----------------------------------------------------------------------------

-- Functor

instance Functor Variable where
  fmap f (Wire v a) = Wire v (f a)
  fmap f (Register v a) = Register v (f a)

instance Functor Port where
  fmap f (Input v a) = Input (f <$> v) (f a)
  fmap f (Output v a) = Output (f <$> v) (f a)

instance Functor e => Functor (Event e) where
  fmap f (PosEdge e a) = PosEdge (f <$> e) (f a)
  fmap f (NegEdge e a) = NegEdge (f <$> e) (f a)
  fmap f (Star a) = Star (f a)

instance Functor Expr where
  fmap f Constant{..} = Constant{ exprData = f exprData, ..}
  fmap f Variable{..} = Variable{ exprData = f exprData, ..}
  fmap f UF{..} = UF { ufArgs   = fmap f <$> ufArgs
                     , exprData = f exprData
                     , ..
                     }
  fmap f IfExpr{..} = IfExpr { ifExprCondition = f <$> ifExprCondition
                             , ifExprThen      = f <$> ifExprThen
                             , ifExprElse      = f <$> ifExprElse
                             , exprData        = f exprData
                             }
  fmap f Str{..} = Str{ exprData = f exprData, ..}
  fmap f Select{..} = Select{ selectVar     = f <$> selectVar
                            , selectIndices = fmap f <$> selectIndices
                            , exprData      = f exprData
                            }

instance Functor e => Functor (Stmt e) where
  fmap f Block{..} = Block{ blockStmts = fmap f <$> blockStmts
                          , stmtData   = f stmtData
                          }
  fmap f Assignment{..} = Assignment{ assignmentLhs = f <$> assignmentLhs
                                    , assignmentRhs = f <$> assignmentRhs
                                    , stmtData      = f stmtData
                                    , ..
                                    }
  fmap f IfStmt{..} = IfStmt { ifStmtCondition = f <$> ifStmtCondition
                             , ifStmtThen      = f <$> ifStmtThen
                             , ifStmtElse      = f <$> ifStmtElse
                             , stmtData        = f stmtData
                             }
  fmap f ModuleInstance{..} = ModuleInstance{ moduleInstancePorts = HM.map (fmap f) moduleInstancePorts
                                            , stmtData = f stmtData
                                            , ..
                                            }
  fmap f (Skip a) = Skip (f a)

instance (Functor e, Functor (s e)) => Functor (AlwaysBlock s e) where
  fmap f AlwaysBlock{..} = AlwaysBlock{ abEvent = f <$> abEvent
                                      , abStmt  = f <$> abStmt
                                      , abData  = f abData
                                      }

instance (Functor e, Functor (s e)) => Functor (Module s e) where
  fmap f Module{..} = Module{ ports        = fmap f <$> ports
                            , variables    = fmap f <$> variables
                            , gateStmts    = fmap f <$> gateStmts
                            , alwaysBlocks = fmap f <$> alwaysBlocks
                            , moduleData   = f moduleData
                            , ..
                            }
