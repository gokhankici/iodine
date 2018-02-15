{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Verylog.Language.Types where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as M
import           Data.Typeable
import           Text.PrettyPrint hiding (sep)

type Id = String

data IR = Register { varName :: Id }
        | Wire     { varName :: Id }
        | UF       { varName :: Id
                   , ufArgs  :: [Id]
                   }
        | Always   { event      :: Event
                   , alwaysStmt :: Stmt
                   }
        | ContAsgn { caLhs      :: Id
                   , caRhs      :: Id
                   }
        | Source   { sourceName :: Id }
        | Sink     { sinkName   :: Id }

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

data St = St { _registers :: S.HashSet Id
             , _wires     :: S.HashSet Id
             , _ufs       :: M.HashMap Id [Id]
             , _sources   :: S.HashSet Id
             , _sinks     :: S.HashSet Id
             , _irs       :: [IR]
             }

makeLenses ''St

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
  pprint = (renderStyle style{lineLength = 150}) . toDoc

instance PPrint IR where
  toDoc (Register{..})    = text "register(" <> text varName <> text ")."
  toDoc (Wire{..})        = text "wire(" <> text varName <> text ")."
  toDoc (UF{..})          = text "link("
                            <> text varName
                            <> comma
                            <+> brackets (hcat $ punctuate (comma <> space) (text <$> ufArgs))
                            <> text ")."
  toDoc (Always{..})      = text "always(" <> vcat [toDoc event <> comma, toDoc alwaysStmt] <> text ")."
  toDoc (ContAsgn{..})    = text "asn(" <> text caLhs <> comma <+> text caRhs <> text ")."
  toDoc (Source{..}) = text "taint_source(" <> text sourceName <> text ")."
  toDoc (Sink{..})   = text "taint_sink(" <> text sinkName <> text ")."
  
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
  toDoc = vcat . (map toDoc)
