{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Iodine.Transform.Query
  ( constructQuery
  , FInfo
  )
where

import           Control.DeepSeq
import           Control.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict           as HM
import qualified Data.Sequence                 as SQ
import           GHC.Generics hiding (moduleName)
import qualified Language.Fixpoint.Types       as FT
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Trace
import           Polysemy.State
import qualified Text.PrettyPrint.HughesPJ     as PP

import           Iodine.Language.Annotation
import           Iodine.Language.IR
import           Iodine.Language.Types
import           Iodine.Transform.Horn

-- -----------------------------------------------------------------------------
-- solver state
-- -----------------------------------------------------------------------------
data HornClauseId = HornClauseId { hcStmtId :: Int, hcType :: Id }
                  deriving (Show, Generic)

data St = St { _hornConstraints           :: HM.HashMap Integer (FT.SubC HornClauseId)
             , _wellFormednessConstraints :: HM.HashMap FT.KVar (FT.WfC HornClauseId)
             , _bindEnvironment           :: FT.BindEnv
             , _globalConstantLiterals    :: FT.SEnv FT.Sort
             , _qualifiers                :: SQ.Seq FT.Qualifier
             }

makeLenses ''St

-- -----------------------------------------------------------------------------
-- check the verification conditions
-- -----------------------------------------------------------------------------

constructQuery :: G r => L (Module Int) -> Horns -> Sem r FInfo
constructQuery modules horns =
  generateFInfo
    & evalState initialState
    & runReader horns
    & runReader modules

generateFInfo :: FD r => Sem r FInfo
generateFInfo = do
  ask >>= traverse_ generateConstraint
  ask >>= traverse_ generateWFConstraint
  asks afQualifiers >>= traverse_ generateQualifiers
  toFInfo

generateConstraint :: Horn () -> Sem r ()
generateConstraint Horn{..} = undefined

generateWFConstraint :: Module Int -> Sem r ()
generateWFConstraint Module{..} = undefined

generateQualifiers :: Qualifier () -> Sem r ()
generateQualifiers _ = undefined

-- -----------------------------------------------------------------------------
-- helper functions
-- -----------------------------------------------------------------------------

toFInfo :: FD r => Sem r FInfo
toFInfo =
  FT.fi
    <$> (toList <$> gets (^. hornConstraints))
    <*> (toList <$> gets (^. wellFormednessConstraints))
    <*> gets (^. bindEnvironment)
    <*> gets (^. globalConstantLiterals)
    <*> -- distinct constant symbols
        return mempty
    <*> -- set of kvars not to eliminate
        return mempty
    <*> (toList <$> gets (^. qualifiers))
    <*> -- metadata about binders
        return mempty
    <*> -- allow higher order binds?
        return False
    <*> -- allow higher order quals?
        return False
    <*> -- asserts
        return mempty
    <*> -- axiom environment
        return mempty
    <*> -- user defined data declarations
        return mempty

type FInfo = FT.FInfo HornClauseId
type Horns = L (Horn ())

type G r = Members '[Trace, Reader (AnnotationFile ())] r
type FD r
  = (G r, Members '[State St, Reader (L (Module Int)), Reader Horns] r)

initialState :: St
initialState = St mempty mempty mempty mempty mempty

instance FT.Loc HornClauseId where
  srcSpan _ = FT.dummySpan

instance FT.Fixpoint HornClauseId where
  toFix (HornClauseId n t) =
    PP.parens (FT.toFix t) PP.<+> PP.text "stmt id:" PP.<+> PP.int n

instance NFData HornClauseId
