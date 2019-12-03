{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Iodine.Language.NewAnnotation where

import           Iodine.Types

import           Control.Lens
import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HM

data Annotations =
  Annotations { _sources       :: HS.HashSet Id
              , _sinks         :: HS.HashSet Id
              , _initialEquals :: HS.HashSet Id
              , _alwaysEquals  :: HS.HashSet Id
              , _assertEquals  :: HS.HashSet Id
              }

data Qualifier = QImplies Id (L Id)
               | QIff     Id (L Id)
               | QPairs   (L Id)

data AnnotationFile =
  AnnotationFile { _annotations :: HM.HashMap Id Annotations
                 , _qualifiers  :: HM.HashMap Id (L Qualifier)
                 , _topModule   :: Id
                 , _clock       :: Maybe Id
                 }
                 
makeLenses ''Annotations
makeLenses ''AnnotationFile
