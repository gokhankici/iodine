{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Iodine.Language.Annotation
  ( Annotation(..)
  , Qualifier(..)
  , AnnotationFile(..)
  )
where

import Iodine.Language.Types
import GHC.Generics

data Annotation a =
    Source       Id a
  | Sink         Id a
  | Sanitize     (L Id) a
  | SanitizeMod  { annotationModuleName :: Id
                 , annotationVarName    :: Id
                 , annotationData       :: a
                 }
  | SanitizeGlob Id a
  | TaintEq      Id a
  | AssertEq     Id a
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
  | QAssume { qualifierAssume :: L Id
            , qualifierData   :: a
            }
  deriving (Generic, Show, Functor, Foldable, Traversable)

data AnnotationFile a =
  AnnotationFile { afAnnotations :: L (Annotation a)
                 , afQualifiers  :: L (Qualifier a)
                 }
  deriving (Generic, Show, Functor, Foldable, Traversable)
