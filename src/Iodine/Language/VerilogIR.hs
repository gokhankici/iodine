module Iodine.Language.VerilogIR ( Expr (..)
                                 , AssignmentType (..)
                                 , Stmt (..)
                                 , Module (..)
                                 , Port (..)
                                 , Variable (..)
                                 , Event (..)
                                 , AlwaysBlock (..)
                                 ) where

import qualified Data.Text           as T
import qualified Data.Sequence       as SQ
import qualified Data.HashMap.Strict as HM

type Id = T.Text  
type L  = SQ.Seq  

data Expr a = Constant { exprValue :: Id
                       , exprData  :: a
                       }
            | Variable { exprValue     :: Id
                       , varModuleName :: Id
                       , exprData      :: a
                       }
            | UF       { ufName   :: Id
                       , ufArgs   :: L (Expr a) 
                       , exprData :: a
                       }
            | IfExpr   { ifExprCondition :: Expr a
                       , ifExprThen      :: Expr a
                       , ifExprElse      :: Expr a
                       , exprData        :: a
                       }
            | Str      { exprValue :: Id
                       , exprData  :: a
                       }
            | Select   { selectVar     :: Expr a
                       , selectIndices :: L (Expr a)
                       , exprData      :: a
                       }
            deriving (Show)

data AssignmentType = Blocking | NonBlocking | Continuous
                    deriving (Show)

data Stmt a = Block      { blockStmts :: L (Stmt a)
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
            | Skip { stmtData :: a }
            deriving (Show)
  
data Module a = Module { moduleName   :: Id
                       , ports        :: L (Port a)
                       , variables    :: L (Variable a)
                       , gateStmts    :: L (Stmt a)
                       , alwaysBlocks :: L (AlwaysBlock a)
                       , moduleData   :: a
                       }
              deriving (Show)

data Port a = Input  (Variable a) a
            | Output (Variable a) a
            deriving (Show)

data Variable a = Wire     Id a
                | Register Id a
                deriving (Show)

data Event a = PosEdge (Expr a) a
             | NegEdge (Expr a) a
             | Star a
             deriving (Show)

data AlwaysBlock a = AlwaysBlock { abEvent :: Event a
                                 , abStmt  :: Stmt a
                                 , abData  :: a  
                                 }
                   deriving (Show)
