{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Language.AnnotationParser
  ( parseAnnotations
  , AnnotationFile(..)
  )
where

import           Iodine.Language.Annotation
import           Iodine.Language.Types

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as B
import           Data.Foldable
import qualified Data.Sequence              as S

newtype AF = AF (AnnotationFile ())
newtype A = A { getAnnotation :: L (Annotation ()) }
newtype Q = Q { getQualifier  :: Qualifier () }

parseAnnotations :: B.ByteString -> AnnotationFile ()
parseAnnotations bs = do
  case eitherDecode bs of
    Right (AF af) -> af
    Left msg -> error $ "Parsing annotation file failed:\n" ++ msg

instance FromJSON AF where
  parseJSON = withObject "AnnotFile" $ \o ->
    fmap AF $ AnnotationFile <$>
    (join  getAnnotation (o .:? "annotations" .!= S.empty)) <*>
    (fmap2 getQualifier  (o .:? "qualifiers"  .!= S.empty)) <*>
    (o .:? "topmodule" .!= "")
    where
      join f = fmap (concatSeq . fmap f)
      fmap2  = fmap . fmap

instance FromJSON A where
  parseJSON = withObject "Annotation" $ \o -> do
    t :: Id <- o .:  "type"
    mm      <- o .:? "module"
    vs      <- o .:  "variables"
    let r = return . A . (<*> return ())
    case t of
      "source"     -> r (Source <$> vs)
      "sink"       -> r (Sink <$> vs)
      "initial_eq" -> case mm of
                        Nothing -> r (S.singleton $ Sanitize vs)
                        Just m  -> r (SanitizeMod m <$> vs)
      "always_eq"  -> case mm of
                        Nothing -> r (SanitizeGlob <$> vs)
                        Just m  -> typeMismatch "always_eq does not support modules (yet)" (toJSON m)
      "assert_eq"  -> case mm of
                        Nothing -> r (AssertEq <$> vs)
                        Just m  -> typeMismatch "assert_eq does not support modules (yet)" (toJSON m)
      _            -> typeMismatch (T.unpack $ "unknown qualifier type: " <> t) (toJSON t)

instance FromJSON Q where
  parseJSON = withObject "Qualifier" $ \o -> do
    t :: Id <- o .:  "type"
    let r = fmap Q . (<*> return ())
    case t of
      "implies" -> r (QImplies <$> o .: "lhs" <*> o .: "rhs")
      "iff"     -> r (QIff <$> o .: "lhs" <*> o .: "rhs")
      "pairs"   -> r (QPairs <$> o .: "variables")
      "assume"  -> r (QAssume <$> o .: "variables")
      _         -> typeMismatch (T.unpack $ "unknown qualifier type: " <> t) (toJSON t)

concatSeq :: Foldable t => t (L a) -> L a
concatSeq = foldl' (S.><) S.empty
