{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData        #-}

module Iodine.Language.Annotation where

import           GHC.Generics
import           Iodine.Types

data Annotation a =
    Source       { getSourceVar   :: Id
                 , annotationData :: a
                 }
  | Sink         { getSourceVar   :: Id
                 , annotationData :: a
                 }
  | Sanitize     (L Id) a
  | SanitizeMod  { annotationModuleName :: Id
                 , annotationVarName    :: Id
                 , annotationData       :: a
                 }
  | SanitizeGlob Id a
  | AssertEq { annotationVarName :: Id
             , annotationData    :: a
             }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data Qualifier a =
    QImplies { qualifierLhs  :: Id
             , qualifierRhss :: L Id
             , qualifierData :: a
             }
  | QIff { qualifierLhs  :: Id
         , qualifierRhss :: L Id
         , qualifierData :: a
         }
  | QPairs { qualifierEqs  :: L Id
           , qualifierData :: a
           }
  deriving (Generic, Show, Functor, Foldable, Traversable)

data AnnotationFile a =
  AnnotationFile { afAnnotations :: L (Annotation a)
                 , afQualifiers  :: L (Qualifier a)
                 , afTopModule   :: Id
                 }
  deriving (Generic, Show, Functor, Foldable, Traversable)

isSource :: Annotation a -> Bool
isSource = \case Source{..} -> True; _ -> False
