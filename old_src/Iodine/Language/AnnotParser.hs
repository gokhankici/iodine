{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Language.AnnotParser ( parseAnnotations
                                   ) where

import           Iodine.Language.Types
import           Iodine.Solver.FP.Types

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy       as B
import           Data.Foldable
import qualified Data.Sequence              as S
import           Data.Sequence (Seq)

data AnnotFile =
  AnnotFile { annotations :: Seq Annotation
            , qualifiers  :: Seq FPQualifier
            }
newtype A = A { getAnnotation :: Seq Annotation }
newtype Q = Q { getQualifier  :: FPQualifier }

parseAnnotations :: B.ByteString -> (Seq Annotation, Seq FPQualifier)
parseAnnotations bs = do
  case eitherDecode bs of
    Right af -> ( annotations af, qualifiers af)
    Left msg -> error $ "Parsing annotation file failed:\n" ++ msg

instance FromJSON AnnotFile where
  parseJSON = withObject "AnnotFile" $ \o ->
    AnnotFile <$>
    (join  getAnnotation (o .:? "annotations" .!= S.empty)) <*>
    (fmap2 getQualifier  (o .:? "qualifiers"  .!= S.empty))
    where
      join f = fmap (concatSeq . fmap f)
      fmap2  = fmap . fmap

instance FromJSON A where
  parseJSON = withObject "Annotation" $ \o -> do
    t :: Id <- o .:  "type"
    mm      <- o .:? "module"
    vs      <- o .:  "variables"
    let r = return . A
    case t of
      "source"     -> r (Source <$> vs)
      "sink"       -> r (Sink <$> vs)
      "initial_eq" -> case mm of
                        Nothing -> r (S.singleton $ Sanitize vs)
                        Just m  -> r (SanitizeMod m <$> vs)
      "always_eq"  -> case mm of
                        Nothing -> r (SanitizeGlob <$> vs)
                        Just m  -> typeMismatch "always_eq does not support modules (yet)" (toJSON m)
      "assert_eq"  -> r (AssertEq <$> vs)
      _            -> typeMismatch "unknown annotation type" (toJSON t)

instance FromJSON Q where
  parseJSON = withObject "Qualifier" $ \o -> do
    t :: Id <- o .:  "type"
    let r = fmap Q
    case t of
      "implies" -> r (QualifImp <$> o .: "lhs" <*> o .: "rhs")
      "iff"     -> r (QualifIff <$> o .: "lhs" <*> o .: "rhs")
      "pairs"   -> r (QualifPairs <$> o .: "variables")
      "assume"  -> r (QualifAssume <$> o .: "variables")
      _         -> typeMismatch "unknown qualifier type" (toJSON t)

concatSeq :: Foldable t => t (Seq a) -> Seq a
concatSeq = foldl' (S.><) S.empty
