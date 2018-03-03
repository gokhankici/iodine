{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Language.Types where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as M
import           Data.Typeable
import           Text.PrettyPrint hiding (sep)
import           Data.List

--------------------------------------------------------------------------------
-- IR for the formalism
--------------------------------------------------------------------------------

data AlwaysBlock = AB { _aEvent   :: Event
                      , _aStmt    :: Stmt
                      , _aId      :: Int
                      , _aSt      :: St
                      }

--------------------------------------------------------------------------------
-- Intermediary IR after parsing
--------------------------------------------------------------------------------

type Id = String

data IR = Always     { event      :: Event
                     , alwaysStmt :: Stmt
                     }
        | ModuleInst { modInstName :: String
                     , modInstArgs :: [(String,String)] -- formal & actual parameters
                     , modInstSt   :: St
                     }

data Event = Star
           | PosEdge Id
           | NegEdge Id

data Stmt = Block           { blockStmts :: [Stmt] }
          | BlockingAsgn    { lhs        :: Id
                            , rhs        :: Id
                            }
          | NonBlockingAsgn { lhs        :: Id
                            , rhs        :: Id
                            }
          | IfStmt          { ifCond     :: Id
                            , thenStmt   :: Stmt
                            , elseStmt   :: Stmt
                            }
          | Skip

data St = St { _ports     :: [Id]
             , _ufs       :: M.HashMap Id [Id]
             , _sources   :: [Id]
             , _sinks     :: [Id]
             , _sanitize  :: [Id]
             , _irs       :: [IR]
             }

emptySt = St { _ports      = []
             , _ufs        = M.empty
             , _sources    = []
             , _sinks      = []
             , _sanitize   = []
             , _irs        = []
             }

makeLenses ''St 
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
  toDoc (ModuleInst{..})  = text "module" <> vcat [ lparen <+> text modInstName
                                                  , comma  <+> pl (pe <$> modInstArgs)
                                                  , comma  <+> toDoc modInstSt
                                                  , rparen
                                                  ] <> text "."
    where
      pe (x,y) = parens (text x <> comma <+> text y)
      pl = brackets . hsep . (punctuate (text ", "))
                            
  
instance PPrint Stmt where
  toDoc (Block [])     = brackets empty
  toDoc (Block (s:ss)) = vcat $ (lbrack <+> toDoc s) : (((comma <+>) . toDoc) <$> ss) ++ [rbrack]
  toDoc (BlockingAsgn{..}) = 
    text "b_asn(" <> text lhs <> comma <+> text rhs <> rparen
  toDoc (NonBlockingAsgn{..}) = 
    text "nb_asn(" <> text lhs <> comma <+> text rhs <> rparen
  toDoc (IfStmt{..}) = text "ite" <> vcat [ lparen <+> text ifCond
                                          , comma  <+> toDoc thenStmt
                                          , comma  <+> toDoc elseStmt
                                          , rparen
                                          ]
  toDoc Skip = text "skip"

instance PPrint Event where
  toDoc Star          = text "event1(star)"
  toDoc (PosEdge clk) = text "event2(posedge," <> text clk <> rparen
  toDoc (NegEdge clk) = text "event2(negedge," <> text clk <> rparen

instance PPrint a => PPrint [a] where
  toDoc = cat . (map toDoc)

instance PPrint St where
  toDoc st = vcat $ stDoc : space : st^.irs.to (map toDoc)
    where
      stDoc = text "St" <+>
              vcat [ lbrace <+> text "ports" <+> equals <+> st^.ports.to     printList
                   , comma  <+> text "ufs  " <+> equals <+> st^.ufs.to       printMap
                   , comma  <+> text "srcs " <+> equals <+> st^.sources.to   printList
                   , comma  <+> text "sinks" <+> equals <+> st^.sinks.to     printList
                   , comma  <+> text "sntz " <+> equals <+> st^.sanitize.to  printList
                   , rbrace
                   ]

instance Show St where
  show = pprint

instance PPrint AlwaysBlock where
  toDoc a = text "always(" <> vcat [ comment "ports   " <+> printList (a^.aSt^.ports) <> comma
                                   , comment "ufs     " <+> printMap  (a^.aSt^.ufs) <> comma
                                   , comment "sources " <+> printList (a^.aSt^.sources) <> comma
                                   , comment "sinks   " <+> printList (a^.aSt^.sinks) <> comma
                                   , comment "sanitize" <+> printList (a^.aSt^.sanitize) <> comma
                                   , comment "id      " <+> int (a^.aId) <> comma
                                   , toDoc (a^.aEvent) <> comma
                                   , toDoc (a^.aStmt)
                                   ] <> text ")."
    where
      comment t = text "/*" <+> text t <+> text "*/"

instance Show AlwaysBlock where
  show = pprint

printList   = brackets . text . (intercalate ", ")
mapKV (k,l) = "(" ++ k ++ ", [" ++ (intercalate ", " l) ++ "])"
printMap    = brackets
              . text
              . (intercalate ", ")
              . (map mapKV)
              . M.toList
