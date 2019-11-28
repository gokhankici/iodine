{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module Iodine.Language.IR
  ( Expr (..)
  , AssignmentType (..)
  , Stmt (..)
  , Module (..)
  , Port (..)
  , Variable (..)
  , Event (..)
  , AlwaysBlock (..)
  , getVariables
  )
where

import           Iodine.Language.Types
import           Iodine.Utils

import           Data.Foldable
import qualified Data.HashSet          as HS
import qualified Data.HashMap.Strict   as HM
import           Data.List             (intercalate)
import qualified Data.Sequence         as SQ
import qualified Data.Text             as T
import           GHC.Generics          hiding (moduleName)
import           Text.Printf
import           Data.Hashable
import qualified Text.PrettyPrint      as PP

data Variable =
    Wire {variableName :: Id}
  | Register {variableName :: Id}

data Port =
    Input  { portVariable :: Variable }
  | Output { portVariable :: Variable }

data Expr a =
  Constant { constantValue :: Id
           , exprData      :: a
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
        , exprData :: a
        }
  | Select { selectVar     :: Expr a
           , selectIndices :: L (Expr a)
           , exprData      :: a
           }
  deriving (Eq, Generic, Functor, Foldable, Traversable)

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
  deriving (Generic, Functor, Foldable, Traversable, Eq)

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

class GetVariables m where
  -- return the name of the variables in type m
  getVariables :: m a -> HS.HashSet Id

instance GetVariables Stmt where
  getVariables = \case
    Block {..}          -> mfold getVariables blockStmts
    Assignment {..}     -> mfold getVariables [assignmentLhs, assignmentRhs]
    IfStmt {..}         -> getVariables ifStmtCondition <> mfold getVariables [ifStmtThen, ifStmtElse]
    ModuleInstance {..} -> not_supported
    Skip {..}           -> mempty

instance GetVariables Expr where
  getVariables = \case
    Variable {..} -> HS.singleton varName
    Constant {..} -> mempty
    UF {..}       -> mfold getVariables ufArgs
    IfExpr {..}   -> mfold getVariables [ifExprCondition, ifExprThen, ifExprElse]
    Str {..}      -> mempty
    Select {..}   -> mfold getVariables $ selectVar SQ.<| selectIndices


-- -----------------------------------------------------------------------------
-- Typeclass Instances
-- -----------------------------------------------------------------------------

instance Show Variable where
  show (Wire v)     = printf "(Wire %s)" v
  show (Register v) = printf "(Reg %s)" v

instance Show Port where
  show (Input p)  = printf "(Input %s)" (show p)
  show (Output p) = printf "(Output %s)" (show p)

instance Show a => Show (Expr a) where
  show (Constant c _)   = T.unpack c
  show (Variable v _ a) = printf "%s#%s" v (show a)
  show (UF n es _)      = printf "%s(%s)" n (intercalate ", " $ show <$> toList es)
  show (IfExpr c t e _) = printf "(%s ? %s : %s)" (show c) (show t) (show e)
  show (Str s _)        = T.unpack s
  show (Select v is _)  = printf "%s%s" (show v) (show $ toList is)

instance Show a => Show (Stmt a) where
  show = PP.render . go
    where
      text = PP.text . T.unpack
      nest = PP.nest 2
      go (Block ss a) =
        case ss of
          SQ.Empty          -> go (Skip a)
          s SQ.:<| SQ.Empty -> go s
          _                 -> PP.cat [ PP.lbrace
                                      , nest (vcatSeq go ss)
                                      , PP.rbrace
                                      ]
      go (Assignment t l r _) =
        PP.text (show l) PP.<+>
        PP.text op PP.<+>
        PP.text (show r) PP.<>
        PP.semi
        where op = case t of
                     Blocking    -> "="
                     NonBlocking -> "<="
                     Continuous  -> ":="
      go (IfStmt c t e _) =
        PP.cat [ PP.text "if" PP.<+> PP.parens (PP.text $ show c) PP.<+> PP.lbrace
               , nest $ go t
               , PP.rbrace PP.<+> PP.text "else" PP.<+> PP.lbrace
               , nest $ go e
               , PP.rbrace
               ]
      go (ModuleInstance t n ps _) =
        -- printf "%s %s(%s)" t n (intercalate ", " args)
        text t PP.<+>
        text n PP.<>
        PP.parens (PP.hcat $ PP.punctuate (PP.comma PP.<+> PP.empty) args)
        where
          args =
            HM.foldrWithKey
            (\v e acc -> (text v PP.<+> PP.equals PP.<+> PP.text (show e)) : acc)
            []
            ps
      go (Skip _) = PP.text "skip"

vcatSeq :: (a -> PP.Doc) -> L a -> PP.Doc
vcatSeq f = foldl' (\d a -> d PP.$+$ f a) PP.empty


instance Show a => Show (Event a) where
  show (PosEdge e _) = printf "@(posedge %s)" (show e)
  show (NegEdge e _) = printf "@(negedge %s)" (show e)
  show (Star _)      = "*"

instance Show a => Show (AlwaysBlock a) where
  show (AlwaysBlock e s _) = printf "always %s %s" (show e) (show s)

instance Show a => Show (Module a) where
  show Module{..} = printf "module(%s,\n%s,\n%s,\n%s,\n%s)"
                    moduleName
                    (show $ toList ports)
                    (show $ toList variables)
                    (intercalate "\n" $ show <$> toList gateStmts)
                    (intercalate "\n" $ show <$> toList alwaysBlocks)

instance Hashable a => Hashable (Expr a) where
  hashWithSalt n (Variable v m a) = hashWithSalt n (v,m,a)
  hashWithSalt _ _                = not_supported

instance Hashable a => Hashable (Event a) where
  hashWithSalt n (PosEdge e a) = hashWithSalt n (e, a)
  hashWithSalt n (NegEdge e a) = hashWithSalt n (e, a)
  hashWithSalt n (Star a)      = hashWithSalt n a
