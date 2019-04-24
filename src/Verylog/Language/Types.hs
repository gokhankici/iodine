{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Verylog.Language.Types where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.HashSet             as S
import qualified Data.Text                as T
import           Data.Typeable
import           Text.PrettyPrint hiding (sep)
import           Data.List
import qualified Data.Semigroup as SG
import           Data.Hashable
import qualified Data.Monoid as Mo
import           GHC.Generics hiding (to)
import           Control.DeepSeq
import qualified Data.Yaml  as Y
import qualified Data.Sequence as SQ
import           Data.Foldable

--------------------------------------------------------------------------------
-- IR for the formalism
--------------------------------------------------------------------------------

type Id = T.Text

type BlockMetadata = BlockMetadataA Id
data BlockMetadataA a =
  BlockMetadata { _mRegisters   :: S.HashSet a
                , _mWires       :: S.HashSet a
                , _mRegReadSet  :: S.HashSet a
                , _mRegWriteSet :: S.HashSet a
                }
  deriving (Generic)

type AlwaysBlock = AlwaysBlockA Id
data AlwaysBlockA a =
  AB { _aEvent   :: EventA a
     , _aStmt    :: StmtA a
     , _aId      :: Int -- two blocks are equal, if this field is the same. be careful !
     , _aSt      :: StA a
     , _aMd      :: BlockMetadataA a
     , _aLoc     :: (Id, Id) -- Module & instance name
     }
  deriving (Generic)

--------------------------------------------------------------------------------
-- Intermediary IR after parsing
--------------------------------------------------------------------------------

type Port = PortA Id
data PortA a =
    Input  { portName :: a }
  | Output { portName :: a }
  deriving (Eq, Generic)

type Var = VarA Id
data VarA a =
    Register { varName :: a }
  | Wire     { varName :: a }
  deriving (Eq, Generic)

type IR = IRA Id
data IRA a =
    Always     { event      :: EventA a
               , alwaysStmt :: StmtA a
               , alwaysLoc  :: (Id, Id) -- Module & instance name
               }
  | ModuleInst { modInstName :: Id
               , modParams   :: [PortA a] -- formal parameters
               , modInstSt   :: StA a
               }
  deriving (Generic)

type Event = EventA Id
data EventA a =
    Star
  | PosEdge { eventVar :: a }
  | NegEdge { eventVar :: a }
  | Assign
  deriving (Eq, Generic)

-- Verilog expression
type VExpr = VExprA Id
data VExprA a =
    VVar { vVarName  :: a }
  | VUF  { vVarName  :: a
         , vFuncName :: a
         , vFuncArgs :: (SQ.Seq (VExprA a))
         }
  deriving (Eq, Generic)

type Stmt  = StmtA Id
data StmtA a =
    Block           { blockStmts :: [StmtA a] }
  | BlockingAsgn    { lhs        :: a
                    , rhs        :: VExprA a
                    }
  | NonBlockingAsgn { lhs        :: a
                    , rhs        :: VExprA a
                    }
  | IfStmt          { ifCond     :: VExprA a
                    , thenStmt   :: StmtA a
                    , elseStmt   :: StmtA a
                    }
  | Skip
  deriving (Generic)

type Annotation = AnnotationA Id
data AnnotationA a =
    Source       a
  | Sink         a
  | Sanitize     [a]
  | SanitizeMod  { annotModuleName :: a
                 , annotVarName    :: a
                 }
  | SanitizeGlob a
  | TaintEq      a
  | AssertEq     a
  deriving (Show)

type AnnotSt = AnnotStA Id
data AnnotStA a =
  AnnotSt { _sources      :: S.HashSet a
          , _sinks        :: S.HashSet a
          , _taintEq      :: S.HashSet a
          , _assertEq     :: S.HashSet a
          , _sanitize     :: S.HashSet a
          , _sanitizeGlob :: S.HashSet a
          }
  deriving (Generic)

data AnnotStFile = AnnotStFile { _goodAnnot :: AnnotSt
                               , _badAnnot  :: AnnotSt
                               }
                 deriving (Generic)

type St = StA Id
data StA a =
  St { _ports :: SQ.Seq (VarA a)
     , _irs   :: SQ.Seq (IRA a)
     }
  deriving (Generic)

type BeSet a = (Eq a, Hashable a)

emptyAnnotSt :: BeSet a => AnnotStA a
emptyAnnotSt = AnnotSt { _sources      = mempty
                       , _sinks        = mempty
                       , _assertEq     = mempty
                       , _taintEq      = mempty
                       , _sanitize     = mempty
                       , _sanitizeGlob = mempty
                       }
emptySt :: StA a
emptySt = St { _ports = mempty
             , _irs   = mempty
             }

makeLenses ''AnnotStA
makeLenses ''AnnotStFile
makeLenses ''StA
makeLenses ''BlockMetadataA
makeLenses ''AlwaysBlockA

done_atom :: Id
done_atom = "done"

runIRs :: (IR -> State St a) -> State St (SQ.Seq a)
runIRs f = use irs >>= sequence . (fmap f)

runIRs_ :: (IR -> State St a) -> State St ()
runIRs_ f = use irs >>= sequence_ . (fmap f)

readIRs :: St -> (IR -> Reader St a) -> SQ.Seq a
readIRs st f = st^.irs.to (fmap (r . f))
  where
    r m = runReader m st

data PassError = PassError !String
               | CycleError { cycleStr      :: !String
                            , cycleErrorStr :: !String
                            }
               deriving (Show, Typeable)

instance Exception PassError


-- -----------------------------------------------------------------------------
-- Pretty printing
-- -----------------------------------------------------------------------------

class PPrint a where
  toDoc :: a -> Doc

  pprint :: a -> String
  pprint = (renderStyle style{ lineLength     = 150
                             , ribbonsPerLine = 1.2
                             }) . toDoc

instance PPrint T.Text where
  toDoc = text . T.unpack

instance PPrint Int where
  toDoc = int

instance PPrint a => PPrint (VExprA a) where
  toDoc (VVar v)   = toDoc v
  toDoc (VUF {..}) = toDoc vFuncName <> parens (hsep $ punctuate comma (toDoc <$> toList vFuncArgs))

instance PPrint a => PPrint (EventA a) where
  toDoc Star          = text "event1(star)"
  toDoc Assign        = text "assign"
  toDoc (PosEdge clk) = text "event2(posedge," <> toDoc clk <> rparen
  toDoc (NegEdge clk) = text "event2(negedge," <> toDoc clk <> rparen


instance PPrint a => PPrint (IRA a) where
  toDoc (Always{..})      = text "always(" <> vcat [toDoc event <> comma, toDoc alwaysStmt] <> text ")."
  toDoc (ModuleInst{..})  = text "module" <> vcat [ lparen <+> id2Doc modInstName
                                                  , comma  <+> pl ((toDoc . portName) <$> modParams)
                                                  , comma  <+> toDoc modInstSt
                                                  , rparen
                                                  ] <> text "."
    where
      pl = brackets . hsep . (punctuate (text ", "))


instance PPrint a => PPrint (StmtA a) where
  toDoc (Block [])     = brackets empty
  toDoc (Block (s:ss)) = vcat $ (lbrack <+> toDoc s) : (((comma <+>) . toDoc) <$> ss) ++ [rbrack]
  toDoc (BlockingAsgn{..}) =
    text "b_asn(" <> toDoc lhs <> comma <+> toDoc rhs <> rparen
  toDoc (NonBlockingAsgn{..}) =
    text "nb_asn(" <> toDoc lhs <> comma <+> toDoc rhs <> rparen
  toDoc (IfStmt{..}) = text "ite" <> vcat [ lparen <+> toDoc ifCond
                                          , comma  <+> toDoc thenStmt
                                          , comma  <+> toDoc elseStmt
                                          , rparen
                                          ]
  toDoc Skip = text "skip"

-- instance PPrint a => PPrint [a] where
--   toDoc as = brackets $ hsep $ punctuate comma (map toDoc as)

instance PPrint a => PPrint (SQ.Seq a) where
  toDoc as = brackets $ hsep $ punctuate comma (toList $ fmap toDoc as)

instance PPrint a => PPrint (VarA a) where
  toDoc (Register r) = text "register" <> parens (toDoc r)
  toDoc (Wire w)     = text "wire" <> parens (toDoc w)

instance PPrint a => PPrint (StA a) where
  toDoc st = vcat $ stDoc : space : st^.irs.(to (fmap toDoc)).(to toList)
    where
      stDoc = text "St" <+>
              vcat [ lbrace <+> text "ports" <+> equals <+> st^.ports.to toDoc
                   , rbrace
                   ]

instance PPrint a => PPrint (AlwaysBlockA a) where
  toDoc a = text "always(" <> vcat [ comment "id       " <+> int (a^.aId)            <> comma
                                   , comment "mod name " <+> id2Doc (a^.aLoc^._1)    <> comma
                                   , comment "inst name" <+> id2Doc (a^.aLoc^._2)    <> comma
                                   , comment "ports    " <+> toDoc (a^.aSt^.ports)   <> comma
                                   , toDoc (a^.aEvent)                               <> comma
                                   , toDoc (a^.aStmt)
                                   ] <> text ")."
    where
      comment t = text "/*" <+> text t <+> text "*/"

instance PPrint a => PPrint (AnnotStA a) where
  toDoc a = text "annots" <> vcat [ lparen <+> text "sources:  " <+> printSet (a^.sources)
                                  , comma  <+> text "sinks:    " <+> printSet (a^.sinks)
                                  , comma  <+> text "taint eq: " <+> printSet (a^.taintEq)
                                  , comma  <+> text "assert eq:" <+> printSet (a^.assertEq)
                                  , comma  <+> text "init eq:  " <+> printSet (a^.sanitize)
                                  , comma  <+> text "always eq:" <+> printSet (a^.sanitizeGlob)
                                  , rparen
                                  ]

instance PPrint a => PPrint [a] where
  toDoc = brackets . cat . (punctuate comma) . fmap toDoc

printSet :: PPrint a => S.HashSet a -> Doc
printSet = toDoc . S.toList

-- Show instances
instance PPrint a => Show (VExprA a) where
  show = pprint
instance PPrint a => Show (StmtA a) where
  show = pprint
instance PPrint a => Show (IRA a) where
  show = pprint
instance PPrint a => Show (EventA a) where
  show = pprint
instance PPrint a => Show (AnnotStA a) where
  show = pprint
instance PPrint a => Show (StA a) where
  show = pprint
instance PPrint a => Show (AlwaysBlockA a) where
  show = pprint

-- NFData instances
instance NFData a => NFData (EventA a)
instance NFData a => NFData (VExprA a)
instance NFData a => NFData (StmtA a)
instance NFData a => NFData (VarA a)
instance NFData a => NFData (PortA a)
instance NFData a => NFData (IRA a)
instance NFData a => NFData (AnnotStA a)
instance NFData a => NFData (StA a)
instance NFData a => NFData (BlockMetadataA a)
instance NFData a => NFData (AlwaysBlockA a)

instance (Eq a, Hashable a) => SG.Semigroup (AnnotStA a) where
  m1 <> m2 =
    AnnotSt { _sources      = jn_set sources
            , _sinks        = jn_set sinks
            , _taintEq      = jn_set taintEq
            , _assertEq     = jn_set assertEq
            , _sanitize     = jn_set sanitize
            , _sanitizeGlob = jn_set sanitizeGlob
            }
    where
      jn_set fld = (m1 ^. fld) Mo.<> (m2 ^. fld)

instance BeSet a => SG.Semigroup (StA a) where
  m1 <> m2 =
    St { _ports = jn_seq ports
       , _irs   = (m1 ^. irs) Mo.<> (m2 ^. irs)
       }
    where
      jn_seq fld = seqNub $ (m1 ^. fld) SQ.>< (m2 ^. fld)

instance (Eq a, Hashable a) => Mo.Monoid (AnnotStA a) where
  mempty  = emptyAnnotSt
  mappend = (SG.<>)

instance (Eq a, Hashable a) => Mo.Monoid (StA a) where
  mempty  = emptySt
  mappend = (SG.<>)

getRegisters :: AlwaysBlockA a -> SQ.Seq a
getRegisters a = fmap varName $ SQ.filter isRegister (a ^. aSt ^. ports)

isRegister :: VarA a -> Bool
isRegister (Register _) = True
isRegister (Wire _)     = False

type IdSeq = SQ.Seq Id

class FoldVariables m where
  foldVariables :: m a -> SQ.Seq a

  foldVariablesSet :: (Eq a , Hashable a) => m a -> S.HashSet a
  foldVariablesSet = foldl' (flip S.insert) mempty . foldVariables

instance FoldVariables VExprA where
  foldVariables (VVar v)   = SQ.singleton v
  foldVariables (VUF {..}) = foldl' f SQ.empty vFuncArgs
    where
      f :: (FoldVariables m) => SQ.Seq a -> m a -> SQ.Seq a
      f vs a = let vs2 = foldVariables a
               in  seq vs2 (vs SQ.>< vs2)

instance FoldVariables StmtA where
  foldVariables Skip                  = SQ.empty
  foldVariables (BlockingAsgn l r)    = l SQ.<| foldVariables r
  foldVariables (NonBlockingAsgn l r) = l SQ.<| foldVariables r
  foldVariables (IfStmt c t e)        = foldVariables c SQ.><
                                        foldVariables t SQ.><
                                        foldVariables e
  foldVariables (Block ss)            = foldl' (SQ.><) SQ.empty (foldVariables <$> ss)

instance FoldVariables IRA where
  foldVariables (Always _ s _) = foldVariables s
  foldVariables _              = throw (PassError "foldVariables called on non-always block")

isClk :: EventA a -> Bool
isClk Assign      = False
isClk Star        = False
isClk (NegEdge _) = True
isClk (PosEdge _) = True

instance Hashable (AlwaysBlockA a) where
  hashWithSalt n (AB{..}) = hashWithSalt n _aId

instance Eq (AlwaysBlockA a) where
  a1 == a2 = (a1 ^. aId) == (a2 ^. aId)

instance BeSet a => Mo.Monoid (BlockMetadataA a) where
  mempty = BlockMetadata { _mRegisters   = mempty
                         , _mWires       = mempty
                         , _mRegReadSet  = mempty
                         , _mRegWriteSet = mempty
                         }
  m1 `mappend` m2 = BlockMetadata { _mRegisters   = j mRegisters
                                  , _mWires       = j mWires
                                  , _mRegReadSet  = j mRegReadSet
                                  , _mRegWriteSet = j mRegWriteSet
                                  }
    where
      j o = (m1^.o) `S.union` (m2^.o)

instance Show a => Show (PortA a) where
  show (Input  i) = "input("  ++ show i ++ ")"
  show (Output o) = "output(" ++ show o ++ ")"

instance Hashable a => Hashable (PortA a) where
  hashWithSalt n (Input s)  = hashWithSalt n ("input" :: Id, s)
  hashWithSalt n (Output s) = hashWithSalt n ("output":: Id, s)

instance Show a => Show (VarA a) where
  show (Register r) = "register(" ++ show r ++ ")"
  show (Wire     w) = "wire("     ++ show w ++ ")"


instance Hashable a => Hashable (VarA a) where
  hashWithSalt n (Register s) = hashWithSalt n ("register" :: Id, s)
  hashWithSalt n (Wire s)     = hashWithSalt n ("wire" :: Id, s)


id2Doc :: Id -> Doc
id2Doc = text . T.unpack

id2Str :: Id -> String
id2Str = T.unpack

str2Id :: String -> Id
str2Id = T.pack

idAppend :: Id -> Id -> Id
idAppend = T.append

idCons :: Char -> Id -> Id
idCons = T.cons

instance Monoid AnnotStFile where
  mempty = AnnotStFile mempty mempty
  m1 `mappend` m2 =
    over goodAnnot (`mappend` (m2^.goodAnnot)) .
    over badAnnot  (`mappend` (m2^.badAnnot)) $
    m1

instance Y.ToJSON AnnotStFile where
  toJSON af = Y.object [ "good" Y..= h (af^.goodAnnot)
                       , "bad"  Y..= h (af^.badAnnot)
                       ]
    where
      h a = Y.object [ f a "init_eq"   sanitize
                     , f a "always_eq" sanitizeGlob
                     ]
      f a name getter = name Y..= (a ^. getter)

instance Y.FromJSON AnnotStFile where
  parseJSON v = do
    ga <- getAnnot "good"
    ba <- getAnnot "bad"
    return $
      mempty &
      goodAnnot .~ ga &
      badAnnot  .~ ba

    where
      getAnnot k = Y.withObject "expected object" (\o -> (o Y..: k) >>= annotParser) v

      f o name setter a = (\x -> set setter x a) <$> (o Y..: name)

      annotParser o = do
        return mempty
        >>=
        f o "init_eq" sanitize
        >>=
        f o "always_eq" sanitizeGlob

encodeAnnotStFile :: FilePath -> AnnotStFile -> IO ()
encodeAnnotStFile = Y.encodeFile

decodeAnnotStFile :: MonadIO m => FilePath -> m AnnotStFile
decodeAnnotStFile = Y.decodeFileThrow

vexprPortSeq :: VExpr -> IdSeq
vexprPortSeq = foldVariables

vexprPortSet :: VExpr -> S.HashSet Id
vexprPortSet = foldVariablesSet

stmtCollectVExpr :: Stmt -> SQ.Seq VExpr
stmtCollectVExpr (Block{..}) = go blockStmts
  where
    go []     = mempty
    go (s:ss) = stmtCollectVExpr s SQ.>< go ss
stmtCollectVExpr (BlockingAsgn{..}) = SQ.singleton rhs
stmtCollectVExpr (NonBlockingAsgn{..}) = SQ.singleton rhs
stmtCollectVExpr (IfStmt{..}) = ifCond SQ.<|
                                stmtCollectVExpr thenStmt SQ.><
                                stmtCollectVExpr elseStmt
stmtCollectVExpr Skip = mempty

seqNub :: (Hashable a, Eq a) => SQ.Seq a -> SQ.Seq a
seqNub = f2seq . seq2set

f2seq :: Foldable t => t a -> SQ.Seq a
f2seq = foldl' (SQ.|>) mempty

seq2set :: (Hashable a, Eq a) => SQ.Seq a -> S.HashSet a
seq2set = foldl' (flip S.insert) mempty
