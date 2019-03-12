{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Verylog.Language.Types where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as M
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

--------------------------------------------------------------------------------
-- IR for the formalism
--------------------------------------------------------------------------------

data BlockMetadata = BlockMetadata { _mRegisters   :: S.HashSet Id
                                   , _mWires       :: S.HashSet Id
                                   , _mRegReadSet  :: S.HashSet Id
                                   , _mRegWriteSet :: S.HashSet Id
                                   }
                   deriving (Generic)

data AlwaysBlock = AB { _aEvent   :: Event
                      , _aStmt    :: Stmt
                      , _aId      :: Int -- to blocks are equal, if this field is the same. be careful !
                      , _aSt      :: St
                      , _aMd      :: BlockMetadata
                      , _aLoc     :: (Id, Id) -- Module & instance name
                      }
                   deriving (Generic)

--------------------------------------------------------------------------------
-- Intermediary IR after parsing
--------------------------------------------------------------------------------

type Id = T.Text

data Port = Input  { portName :: ! Id }
          | Output { portName :: ! Id }
          deriving (Eq, Generic)

data Var = Register { varName :: ! Id }
         | Wire     { varName :: ! Id }
         deriving (Eq, Generic)

data IR = Always     { event      :: ! Event
                     , alwaysStmt :: ! Stmt
                     , alwaysLoc  :: ! (Id, Id) -- Module & instance name
                     }
        | ModuleInst { modInstName :: ! Id
                     , modParams   :: ! [Port] -- formal parameters
                     , modInstSt   :: ! St
                     }
        deriving (Generic)

data Event = Star
           | PosEdge { eventVar :: Id }
           | NegEdge { eventVar :: Id }
           | Assign
           deriving (Eq, Generic)

data Stmt = Block           { blockStmts :: ! [Stmt] }
          | BlockingAsgn    { lhs        :: ! Id
                            , rhs        :: ! Id
                            }
          | NonBlockingAsgn { lhs        :: ! Id
                            , rhs        :: ! Id
                            }
          | IfStmt          { ifCond     :: ! Id
                            , thenStmt   :: ! Stmt
                            , elseStmt   :: ! Stmt
                            }
          | Skip
          deriving (Generic)

type UFMap = M.HashMap Id (Id, [Id])

data Annotation
  = Source       Id
  | Sink         Id
  | Sanitize     [Id]
  | SanitizeMod  { annotModuleName :: Id
                 , annotVarName    :: Id
                 }
  | SanitizeGlob Id
  | TaintEq      Id
  | AssertEq     Id
  deriving (Show)

data AnnotSt = AnnotSt { _sources      :: S.HashSet Id
                       , _sinks        :: S.HashSet Id
                       , _taintEq      :: S.HashSet Id
                       , _assertEq     :: S.HashSet Id
                       , _sanitize     :: S.HashSet Id
                       , _sanitizeGlob :: S.HashSet Id
                       }
               deriving (Generic)

data AnnotStFile = AnnotStFile { _goodAnnot :: AnnotSt
                               , _badAnnot  :: AnnotSt
                               }
                 deriving (Generic)

data St = St { _ports :: [Var]
             , _ufs   :: UFMap
             , _irs   :: [IR]
             }
          deriving (Generic)

emptyAnnotSt :: AnnotSt
emptyAnnotSt = AnnotSt { _sources      = mempty
                       , _sinks        = mempty
                       , _assertEq     = mempty
                       , _taintEq      = mempty
                       , _sanitize     = mempty
                       , _sanitizeGlob = mempty
                       }
emptySt :: St
emptySt = St { _ports = mempty
             , _ufs   = mempty
             , _irs   = mempty
             }

makeLenses ''AnnotSt
makeLenses ''AnnotStFile
makeLenses ''St
makeLenses ''BlockMetadata
makeLenses ''AlwaysBlock

done_atom :: Id
done_atom = "done"

runIRs :: (IR -> State St a) -> State St [a]
runIRs f = use irs >>= sequence . (map f)

runIRs_ :: (IR -> State St a) -> State St ()
runIRs_ f = use irs >>= sequence_ . (map f)

readIRs :: St -> (IR -> Reader St a) -> [a]
readIRs st f = st^.irs.to (map (r . f))
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

instance PPrint IR where
  toDoc (Always{..})      = text "always(" <> vcat [toDoc event <> comma, toDoc alwaysStmt] <> text ")."
  toDoc (ModuleInst{..})  = text "module" <> vcat [ lparen <+> id2Doc modInstName
                                                  , comma  <+> pl ((id2Doc . portName) <$> modParams)
                                                  , comma  <+> toDoc modInstSt
                                                  , rparen
                                                  ] <> text "."
    where
      pl = brackets . hsep . (punctuate (text ", "))


instance PPrint Stmt where
  toDoc (Block [])     = brackets empty
  toDoc (Block (s:ss)) = vcat $ (lbrack <+> toDoc s) : (((comma <+>) . toDoc) <$> ss) ++ [rbrack]
  toDoc (BlockingAsgn{..}) =
    text "b_asn(" <> id2Doc lhs <> comma <+> id2Doc rhs <> rparen
  toDoc (NonBlockingAsgn{..}) =
    text "nb_asn(" <> id2Doc lhs <> comma <+> id2Doc rhs <> rparen
  toDoc (IfStmt{..}) = text "ite" <> vcat [ lparen <+> id2Doc ifCond
                                          , comma  <+> toDoc thenStmt
                                          , comma  <+> toDoc elseStmt
                                          , rparen
                                          ]
  toDoc Skip = text "skip"

instance PPrint Event where
  toDoc Star          = text "event1(star)"
  toDoc Assign        = text "assign"
  toDoc (PosEdge clk) = text "event2(posedge," <> id2Doc clk <> rparen
  toDoc (NegEdge clk) = text "event2(negedge," <> id2Doc clk <> rparen

instance PPrint a => PPrint [a] where
  toDoc as = brackets $ hsep $ punctuate comma (map toDoc as)

instance PPrint Var where
  toDoc (Register r) = text "register" <> parens (id2Doc r)
  toDoc (Wire w)     = text "wire" <> parens (id2Doc w)

instance PPrint St where
  toDoc st = vcat $ stDoc : space : st^.irs.to (map toDoc)
    where
      stDoc = text "St" <+>
              vcat [ lbrace <+> text "ports" <+> equals <+> st^.ports.to         toDoc
                   , comma  <+> text "ufs  " <+> equals <+> st^.ufs.to           printMap
                   , rbrace
                   ]

instance PPrint AlwaysBlock where
  toDoc a = text "always(" <> vcat [ comment "id       " <+> int (a^.aId)            <> comma
                                   , comment "mod name " <+> id2Doc (a^.aLoc^._1)    <> comma
                                   , comment "inst name" <+> id2Doc (a^.aLoc^._2)    <> comma
                                   , comment "ports    " <+> toDoc (a^.aSt^.ports)   <> comma
                                   , comment "ufs      " <+> printMap  (a^.aSt^.ufs) <> comma
                                   , toDoc (a^.aEvent)                               <> comma
                                   , toDoc (a^.aStmt)
                                   ] <> text ")."
    where
      comment t = text "/*" <+> text t <+> text "*/"

instance PPrint AnnotSt where
  toDoc a = text "annots" <> vcat [ lparen <+> text "sources:  " <+> printSet (a^.sources)
                                  , comma  <+> text "sinks:    " <+> printSet (a^.sinks)
                                  , comma  <+> text "taint eq: " <+> printSet (a^.taintEq)
                                  , comma  <+> text "assert eq:" <+> printSet (a^.assertEq)
                                  , comma  <+> text "init eq:  " <+> printSet (a^.sanitize)
                                  , comma  <+> text "always eq:" <+> printSet (a^.sanitizeGlob)
                                  , rparen
                                  ]

printSet :: S.HashSet Id -> Doc
printSet = printList . S.toList

printList :: [Id] -> Doc
printList = brackets . text . (intercalate ", ") . fmap id2Str

printMap :: UFMap -> Doc
printMap = brackets
           . text
           . (intercalate ", ")
           . (map mapKV)
           . M.toList
  where
    mapKV (k,(f, l)) = "(" ++ id2Str k ++ ", " ++ id2Str f ++ ", [" ++ (intercalate ", " $ id2Str <$> l) ++ "])"

instance Show IR where
  show = pprint
instance Show Stmt where
  show = pprint
instance Show Event where
  show = pprint
instance Show AnnotSt where
  show = pprint
instance Show St where
  show = pprint
instance Show AlwaysBlock where
  show = pprint

instance NFData Event
instance NFData Stmt
instance NFData Var
instance NFData Port
instance NFData IR
instance NFData AnnotSt
instance NFData St
instance NFData BlockMetadata
instance NFData AlwaysBlock

instance SG.Semigroup AnnotSt where
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

instance SG.Semigroup St where
  m1 <> m2 =
    St { _ports = jn_list ports
       , _ufs   = (m1 ^. ufs) Mo.<> (m2 ^. ufs)
       , _irs   = (m1 ^. irs) Mo.<> (m2 ^. irs)
       }
    where
      jn_list fld =
        S.toList $
        S.fromList (m1 ^. fld) Mo.<>
        S.fromList (m2 ^. fld)

instance Mo.Monoid AnnotSt where
  mempty  = emptyAnnotSt
  mappend = (SG.<>)

instance Mo.Monoid St where
  mempty  = emptySt
  mappend = (SG.<>)

getRegisters :: AlwaysBlock -> [Id]
getRegisters a =
  map varName $ filter isRegister (a ^. aSt ^. ports)

isRegister :: Var -> Bool
isRegister (Register _) = True
isRegister (Wire _)     = False

class FoldVariables a where
  foldVariables :: a -> [Id]

instance FoldVariables Stmt where
  foldVariables (Block ss)            = concatMap foldVariables ss
  foldVariables (BlockingAsgn l r)    = [l, r]
  foldVariables (NonBlockingAsgn l r) = [l, r]
  foldVariables (IfStmt c t e)        = [c] ++ concatMap foldVariables [t,e]
  foldVariables Skip                  = []

instance FoldVariables IR where
  foldVariables (Always _ s _) = foldVariables s
  foldVariables _              = throw (PassError "foldVariables called on non-always block")

isClk :: Event -> Bool
isClk Assign      = False
isClk Star        = False
isClk (NegEdge _) = True
isClk (PosEdge _) = True

instance Hashable AlwaysBlock where
  hashWithSalt n (AB{..}) = hashWithSalt n _aId

instance Eq AlwaysBlock where
  a1 == a2 = (a1 ^. aId) == (a2 ^. aId)

instance Mo.Monoid BlockMetadata where
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
instance Show Port where
  show (Input  i) = "input("  ++ id2Str i ++ ")"
  show (Output o) = "output(" ++ id2Str o ++ ")"

instance Hashable Port where
  hashWithSalt n (Input s)  = hashWithSalt n ("input" :: Id, s)
  hashWithSalt n (Output s) = hashWithSalt n ("output":: Id, s)

instance Show Var where
  show (Register r) = "register(" ++ id2Str r ++ ")"
  show (Wire     w) = "wire("     ++ id2Str w ++ ")"


instance Hashable Var where
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
