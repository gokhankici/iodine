{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

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

import           Data.Monoid ((<>))

data Variable a =
  Wire { variableName :: Id
       , variableData :: a
       }
  | Register {variableName :: Id
             , variableData :: a
             }
  deriving (Show)

data Port a =
  Input  { portVariable :: Variable a
         , portData :: a
         }
  | Output { portVariable :: Variable a
           , portData :: a
           }
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

data Event expr a =
  PosEdge { eventExpr :: expr a
          , eventData :: a
          }
  | NegEdge { eventExpr :: expr a
            , eventData :: a
            }
  | Star { eventData :: a }
  deriving (Show)

data AlwaysBlock stmt expr a =
  AlwaysBlock { abEvent :: Event expr a
              , abStmt  :: stmt expr a
              , abData  :: a
              }
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

-- Functor

instance Functor Variable where
  fmap f Wire{..} = Wire {variableData = f variableData, ..}
  fmap f Register{..} = Register {variableData = f variableData, ..}

instance Functor Port where
  fmap f Input{..} = Input {portVariable = f <$> portVariable, portData = f portData}
  fmap f Output{..} = Output {portVariable = f <$> portVariable, portData = f portData}

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

-- Foldable

instance Foldable Variable where
  foldMap f = f . variableData

instance Foldable Port where
  foldMap f p = foldMap f (portVariable p) <> f (portData p)

instance Foldable e => Foldable (Event e) where
  foldMap f Star{..} = f eventData
  foldMap f PosEdge{..} = foldMap f eventExpr <> f eventData
  foldMap f NegEdge{..} = foldMap f eventExpr <> f eventData

instance Foldable Expr where
  foldMap f Constant{..} = f exprData
  foldMap f Variable{..} = f exprData
  foldMap f UF{..} = foldMap (foldMap f) ufArgs <> f exprData
  foldMap f IfExpr{..} = foldMap f ifExprCondition <>
                         foldMap f ifExprThen <>
                         foldMap f ifExprElse <>
                         f exprData
  foldMap f Str{..} = f exprData
  foldMap f Select{..} = foldMap f selectVar <>
                         foldMap (foldMap f) selectIndices <>
                         f exprData

instance Foldable e => Foldable (Stmt e) where
  foldMap f Block{..} = foldMap (foldMap f) blockStmts <>
                        f stmtData
  foldMap f Assignment{..} = foldMap f assignmentLhs <>
                             foldMap f assignmentRhs <>
                             f stmtData
  foldMap f IfStmt{..} = foldMap f ifStmtCondition <>
                         foldMap f ifStmtThen <>
                         foldMap f ifStmtElse <>
                         f stmtData
  foldMap f ModuleInstance{..} = foldMap (foldMap f) moduleInstancePorts <> f stmtData
  foldMap f (Skip a) = f a

instance (Foldable e, Foldable (s e)) => Foldable (AlwaysBlock s e) where
  foldMap f AlwaysBlock{..} = foldMap f abEvent <> foldMap f abStmt <> f abData

instance (Foldable e, Foldable (s e)) => Foldable (Module s e) where
  foldMap f Module{..} = foldMap (foldMap f) ports <>
                         foldMap (foldMap f) variables <>
                         foldMap (foldMap f) gateStmts <>
                         foldMap (foldMap f) alwaysBlocks <>
                         f moduleData

-- Traversable

instance Traversable Variable where
  traverse m (Wire n a) = Wire n <$> m a 
  traverse m (Register n a) = Register n <$> m a 

instance Traversable Port where
  traverse m (Input v a) = Input <$> traverse m v <*> m a
  traverse m (Output v a) = Output <$> traverse m v <*> m a

instance Traversable e => Traversable (Event e) where
  traverse m (Star a) = Star <$> m a
  traverse m (PosEdge e a) = PosEdge <$> traverse m e <*> m a
  traverse m (NegEdge e a) = NegEdge <$> traverse m e <*> m a

instance Traversable Expr where
  traverse m Constant{..} = Constant constantValue <$> m exprData
  traverse m Variable{..} = Variable varName varModuleName <$> m exprData
  traverse m UF{..} = UF ufName <$> traverse (traverse m) ufArgs <*> m exprData
  traverse m IfExpr{..} = IfExpr <$>
                          traverse m ifExprCondition <*>
                          traverse m ifExprThen <*>
                          traverse m ifExprElse <*>
                          m exprData
  traverse m Str{..} = Str strValue <$> m exprData
  traverse m Select{..} = Select <$>
                          traverse m selectVar <*>
                          traverse (traverse m) selectIndices <*>
                          m exprData

instance Traversable e => Traversable (Stmt e) where
  traverse m Block{..} = Block <$>
                         traverse (traverse m) blockStmts <*>
                         m stmtData
  traverse m Assignment{..} = Assignment assignmentType <$>
                              traverse m assignmentLhs <*>
                              traverse m assignmentRhs <*>
                              m stmtData
  traverse m IfStmt{..} = IfStmt <$>
                          traverse m ifStmtCondition <*>
                          traverse m ifStmtThen <*>
                          traverse m ifStmtElse <*>
                          m stmtData
  traverse m ModuleInstance{..} = ModuleInstance moduleInstanceType moduleInstanceName <$>
                                  traverse (traverse m) moduleInstancePorts <*>
                                  m stmtData
  traverse m (Skip a) = Skip <$> m a

instance (Traversable e, Traversable (s e)) => Traversable (AlwaysBlock s e) where
  traverse m AlwaysBlock{..} = AlwaysBlock <$>
                               traverse m abEvent <*>
                               traverse m abStmt <*>
                               m abData

instance (Traversable e, Traversable (s e)) => Traversable (Module s e) where
  traverse m Module{..} = Module moduleName <$>
                          traverse (traverse m) ports <*>
                          traverse (traverse m) variables <*>
                          traverse (traverse m) gateStmts <*>
                          traverse (traverse m) alwaysBlocks <*>
                          m moduleData
