{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
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
  , ModuleInstance (..)
  , Module (..)
  , Port (..)
  , Variable (..)
  , Event (..)
  , AlwaysBlock (..)
  , getVariables
  , isSummaryStmt
  )
where

import           Iodine.Types
import           Iodine.Utils

import           Data.Foldable
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as SQ
import qualified Data.Text as T
import           GHC.Generics hiding (moduleName)
import           Data.Hashable
import qualified Text.PrettyPrint as PP

data Variable =
    Wire     { variableName :: Id }
  | Register { variableName :: Id }
  deriving (Eq)

data Port =
    Input  { portVariable :: Variable }
  | Output { portVariable :: Variable }
  deriving (Eq)

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

data ModuleInstance a =
  ModuleInstance { moduleInstanceType  :: Id
                 , moduleInstanceName  :: Id
                 , moduleInstancePorts :: HM.HashMap Id (Expr a)
                 , moduleInstanceData  :: a
                 }
  deriving (Generic, Functor, Foldable, Traversable)

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
  | Skip { stmtData :: a }
  | SummaryStmt { summaryType  :: Id -- ^ module name
                , summaryPorts :: HM.HashMap Id (Expr a) -- ^ module instance ports
                , stmtData     :: a
                }
  deriving (Generic, Functor, Foldable, Traversable)

data Event a =
    PosEdge { eventExpr :: Expr a }
  | NegEdge { eventExpr :: Expr a }
  | Star
  deriving (Generic, Functor, Foldable, Traversable, Eq)

data AlwaysBlock a =
    AlwaysBlock { abEvent :: Event a
                , abStmt  :: Stmt a
                }
  deriving (Generic, Functor, Foldable, Traversable)

data Module a =
  Module { moduleName      :: Id
         , ports           :: L Port
         , variables       :: L Variable
         , gateStmts       :: L (Stmt a)
         , alwaysBlocks    :: L (AlwaysBlock a)
         , moduleInstances :: L (ModuleInstance a)
         , moduleData      :: a
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
    Skip {..}           -> mempty
    SummaryStmt{..}     -> HM.foldlWithKey' (\acc p e -> HS.insert p acc <> getVariables e) mempty summaryPorts

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
class ShowIndex a where
  showIndex :: a -> String

instance ShowIndex () where
  showIndex () = ""

instance ShowIndex Int where
  showIndex n  = " #" ++ show n

docIndex :: ShowIndex a => a -> PP.Doc
docIndex = PP.text . showIndex



class Doc a where
  doc :: a -> PP.Doc

sep :: PP.Doc
sep = PP.comma

docList :: Doc a => L a -> PP.Doc
docList l = PP.hsep $ PP.punctuate sep (doc <$> toList l)

nest :: PP.Doc -> PP.Doc
nest = PP.nest 2

vcat :: Doc a => L a -> PP.Doc
vcat = PP.vcat . fmap doc . toList

instance Doc T.Text where
  doc = PP.text . T.unpack

instance Doc Variable where
  doc (Wire v)     = PP.text "wire" PP.<+> doc v PP.<> PP.semi
  doc (Register v) = PP.text "reg " PP.<+> doc v PP.<> PP.semi

instance Doc Port where
  doc (Input p)  = PP.text "input " PP.<+> doc (variableName p) PP.<> PP.semi
  doc (Output p) = PP.text "output" PP.<+> doc (variableName p) PP.<> PP.semi

instance ShowIndex a => Doc (Expr a) where
  doc (Constant c _)   = doc c
  doc (Variable v _ a) = doc v PP.<> docIndex a
  doc (UF n es _)      = doc n PP.<> PP.parens (docList es)
  doc (IfExpr c t e _) = PP.parens $ PP.hsep [doc c, PP.text "?", doc t, PP.colon, doc e]
  doc (Str s _)        = PP.quotes $ doc s
  doc (Select v is _)  = doc v PP.<> PP.brackets (docList is)

instance ShowIndex a => Doc (Event a) where
  doc (PosEdge e) = PP.text "@(posedge " PP.<> doc e PP.<> PP.rparen
  doc (NegEdge e) = PP.text "@(negedge " PP.<> doc e PP.<> PP.rparen
  doc Star        = PP.text "*"

instance ShowIndex a => Doc (Stmt a) where
  doc (Block ss a) =
    case ss of
      SQ.Empty          -> doc (Skip a)
      s SQ.:<| SQ.Empty -> doc s
      _                 -> PP.lbrace PP.$+$
                           nest (vcat ss) PP.$+$
                           PP.rbrace
  doc (Assignment t l r _) =
    doc l PP.<+> PP.text op PP.<+> doc r PP.<> PP.semi
    where op = case t of
                 Blocking    -> "="
                 NonBlocking -> "<="
                 Continuous  -> ":="
  doc (IfStmt c t e _) =
    PP.cat [ PP.text "if" PP.<+> PP.parens (doc c) PP.<+> PP.lbrace
           , nest $ doc t
           , PP.rbrace PP.<+> PP.text "else" PP.<+> PP.lbrace
           , nest $ doc e
           , PP.rbrace
           ]
  doc (Skip _) = PP.text "skip" PP.<> PP.semi
  doc (SummaryStmt t ps _) =
    doc t PP.<> PP.parens (PP.hsep $ PP.punctuate sep args)
    where
      args = docArgs ps

docArgs :: (Doc k, Doc v) => HM.HashMap k v -> [PP.Doc]
docArgs = HM.foldlWithKey' (\acc v e-> (doc v PP.<+> PP.equals PP.<+> doc e) : acc) []

instance ShowIndex a => Doc (ModuleInstance a) where
  doc (ModuleInstance t n ps a) =
    doc t PP.<+> doc n PP.<> PP.parens (PP.hsep $ PP.punctuate sep args) PP.<> docIndex a
    where
      args = docArgs ps

instance ShowIndex a => Doc (AlwaysBlock a) where
  doc (AlwaysBlock e s) =
    PP.sep [ PP.text "always"
             PP.<> PP.text (showIndex $ stmtData s)
             PP.<+> doc e
           , doc s
           ]

instance ShowIndex a => Doc (Module a) where
  doc Module{..} =
    PP.vcat [ PP.text "module" PP.<+> doc moduleName PP.<> PP.parens args PP.<> PP.semi
            , PP.nest 2 contents
            , PP.text "endmodule"
            ]
    where
      contents =
        vcatNL [ vcat ports
               , vcat variables
               , vcatNS gateStmts
               , vcatNS moduleInstances
               , vcatNS alwaysBlocks
               ]
      args =
        PP.hsep $
        PP.punctuate sep (doc . variableName . portVariable <$> toList ports)

      vcatNS :: Doc b => L b -> PP.Doc
      vcatNS = vcatNL . fmap doc . toList

      vcatNL :: [PP.Doc] -> PP.Doc
      vcatNL = PP.vcat . go . filter (not . PP.isEmpty)
        where
          go []     = []
          go [a]    = [a]
          go (a:as) = a : PP.text "" : go as


instance Hashable a => Hashable (Expr a) where
  hashWithSalt n (Variable v m a) = hashWithSalt n (v,m,a)
  hashWithSalt _ _                = notSupported

instance Hashable a => Hashable (Event a) where
  hashWithSalt n (PosEdge e) = hashWithSalt n (1::Int, e)
  hashWithSalt n (NegEdge e) = hashWithSalt n (2::Int, e)
  hashWithSalt n Star        = hashWithSalt n (3::Int)


instance ShowIndex a => Show (Event a) where
  show = PP.render . doc

instance ShowIndex a => Show (Stmt a) where
  show = PP.render . doc

instance ShowIndex a => Show (AlwaysBlock a) where
  show = PP.render . doc

instance ShowIndex a => Show (Module a) where
  show = PP.render . doc

isSummaryStmt :: Stmt a -> Bool
isSummaryStmt SummaryStmt{..} = True
isSummaryStmt _ = False