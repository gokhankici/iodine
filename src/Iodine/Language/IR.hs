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

import           Data.List           (intercalate)
import           Data.Foldable       (toList)
import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HM
import           GHC.Generics        hiding (moduleName)
import           Text.Printf

data Variable =
    Wire {variableName :: Id}
  | Register {variableName :: Id}

data Port =
    Input  { portVariable :: Variable }
  | Output { portVariable :: Variable }

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
  deriving (Generic, Functor, Foldable, Traversable)

data AssignmentType = Blocking | NonBlocking | Continuous
                    deriving (Generic, Eq)

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
             , stmtData        :: a
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
  deriving (Generic, Functor, Foldable, Traversable)

data Event a =
  PosEdge { eventExpr :: Expr a
          , eventData :: a
          }
  | NegEdge { eventExpr :: Expr a
            , eventData :: a
            }
  | Star { eventData :: a }
  deriving (Generic, Functor, Foldable, Traversable)

data AlwaysBlock a =
  AlwaysBlock { abEvent :: Event a
              , abStmt  :: Stmt a
              , abData  :: a
              }
  deriving (Generic, Functor, Foldable, Traversable)

data Module a =
  Module { moduleName   :: Id
         , ports        :: L Port
         , variables    :: L Variable
         , gateStmts    :: L (Stmt a)
         , alwaysBlocks :: L (AlwaysBlock a)
         , moduleData   :: a
         }
  deriving (Generic, Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------
-- Typeclass Instances
-- -----------------------------------------------------------------------------

instance Show Variable where
  show (Wire v) = printf "(Wire %s)" v
  show (Register v) = printf "(Reg %s)" v

instance Show Port where
  show (Input p) = printf "(Input %s)" (show p)
  show (Output p) = printf "(Output %s)" (show p)

instance Show a => Show (Expr a) where
  show (Constant c _)   = T.unpack c
  show (Variable v _ a) = printf "%s#%s" v (show a)
  show (UF n es _)      = printf "(UF %s %s)" n (show $ toList es)
  show (IfExpr c t e _) = printf "(%s ? %s : %s)" (show c) (show t) (show e)
  show (Str s _)        = T.unpack s
  show (Select v is _)  = printf "%s%s" (show v) (show $ toList is)

instance Show a => Show (Stmt a) where
  show (Block ss _) = printf "{ %s }" (intercalate "; " (toList $ show <$> ss))
  show (Assignment t l r _) = printf "%s %s %s" (show l) op (show r)
                              where op = case t of
                                           Blocking -> "="
                                           NonBlocking -> "<="
                                           Continuous -> ":="
  show (IfStmt c t e _) = printf "if( %s ){ %s }else{ %s }" (show c) (show t) (show e)
  show (ModuleInstance t n ps _) = printf "%s %s(%s)" t n (intercalate ", " args)
                                   where args = (\(k,e) -> printf "%s = %s" k (show e)) <$> HM.toList ps
  show (PhiNode l rs _) = printf "%s = phi(%s)" (show l) (intercalate ", " (toList $ show <$> rs))
  show (Skip _) = printf "skip"

instance Show a => Show (Event a) where
  show (PosEdge e _) = printf "@(posedge %s)" (show e)
  show (NegEdge e _) = printf "@(negedge %s)" (show e)
  show (Star _)      = "*"

instance Show a => Show (AlwaysBlock a) where
  show (AlwaysBlock e s _) = printf "always %s %s" (show e) (show s)

instance Show a => Show (Module a) where
  show Module{..} = printf "module(%s, %s, %s, %s, %s)"
                    moduleName
                    (show $ toList ports)
                    (show $ toList variables)
                    (show $ toList gateStmts)
                    (show $ toList alwaysBlocks)
