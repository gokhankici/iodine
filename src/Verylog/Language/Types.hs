{-# language RecordWildCards #-}

module Verylog.Language.Types (
  Id,

  IR    (..),
  Event (..),
  Stmt  (..),

  PPrint (..),

  PassError (..)
  ) where

import Text.PrettyPrint hiding (sep)
import Control.Exception
import Data.Typeable

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

class PPrint a where
  toDoc :: a -> Doc

  pprint :: a -> String
  pprint = render . toDoc

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

data PassError = PassError !String
               deriving (Show, Typeable)

instance Exception PassError
