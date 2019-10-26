{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Language.Parser ( parse
                              , parseWithoutConversion
                              , renderError
                              , IRParseError (..)
                              ) where

import           Control.Arrow
import           Control.Exception
-- import           Control.Lens
import           Control.Monad (void)
-- import           Control.Monad.State.Lazy
import           Data.Char (isLetter, isDigit)
import           Data.Foldable (toList)
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
-- import qualified Data.HashSet               as S
import qualified Data.Text                  as T
import           Data.Typeable
import           Text.Megaparsec            ((<|>))
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
-- import           Text.Printf
import qualified Data.Sequence              as SQ

import qualified Iodine.Language.VerilogIR as VIR

-- import Iodine.Utils
import Iodine.Language.Types
import Iodine.Solver.FP.Types

type ParseInput  = ((FilePath, String), (SQ.Seq Annotation, SQ.Seq FPQualifier))
type ParseOutput = ((St, AnnotSt), SQ.Seq FPQualifier)

type Parser = MP.Parsec MP.SourcePos String
type L = SQ.Seq
type ParsedIR = L (VIR.Module ())

parse :: ParseInput -> ParseOutput
parse = first parseWithoutConversion >>>
        arr (\(o,(a,q)) -> ((o, a),q)) >>>
        first makeState

parseWithoutConversion :: (FilePath, String) -> ParsedIR
parseWithoutConversion (fp, s) = parseWith (many parseModule)
  where
    parseWith p =
      case MP.runParser (whole p) fp s of
        Right e     -> e
        Left bundle -> throw (IRParseError (myParseErrorPretty bundle))

makeState :: (ParsedIR, SQ.Seq Annotation) -> (St, AnnotSt)
makeState = undefined

--------------------------------------------------------------------------------
-- | IR Parser
--------------------------------------------------------------------------------

parseModule :: Parser (VIR.Module ())
parseModule =
  parseTerm "module" $
  VIR.Module
  <$> identifier
  <*> (comma *> list parsePort)
  <*> (comma *> list parseVariable)
  <*> (comma *> list parseStmt)
  <*> (comma *> list parseAlwaysBlock)
  <*> parseData

parsePort :: Parser (VIR.Port ())
parsePort =
  parseTerm "input" (VIR.Input  <$> parseVariable <*> parseData) <|>
  parseTerm "output" (VIR.Output  <$> parseVariable <*> parseData)

parseVariable :: Parser (VIR.Variable ())
parseVariable =
  parseTerm "wire"     (VIR.Wire     <$> identifier <*> parseData) <|>
  parseTerm "register" (VIR.Register <$> identifier <*> parseData)

parseExpr :: Parser (VIR.Expr ())
parseExpr =
  parseTerm "const" (VIR.Constant <$> constVar <*> parseData) <|>
  parseTerm "var" (VIR.Variable <$> identifier <*> (comma *> identifier) <*> parseData) <|>
  parseTerm "uf" (VIR.UF <$> identifier <*> (comma *> list parseExpr) <*> parseData) <|>
  parseTerm "ite_expr" (VIR.IfExpr
                        <$> parseExpr
                        <*> (comma *> parseExpr)
                        <*> (comma *> parseExpr)
                        <*> parseData) <|>
  parseTerm "str" (VIR.Str <$> identifier <*> parseData) <|>
  parseTerm "select" (VIR.Select
                      <$> parseExpr
                      <*> (comma *> list parseExpr)
                      <*> parseData)
  where
    constVar :: Parser Id
    constVar = T.pack <$> MP.many MPC.alphaNumChar

parseStmt :: Parser (VIR.Stmt ())
parseStmt =
  parseTerm "block" (VIR.Block <$> list parseStmt <*> parseData) <|>
  parseAsn "b_asn" VIR.Blocking <|>
  parseAsn "nb_asn" VIR.NonBlocking <|>
  parseAsn "asn" VIR.Continuous <|>
  parseTerm "ite_stmt" (VIR.IfStmt
                        <$> parseExpr
                        <*> (comma *> parseStmt)
                        <*> (comma *> parseStmt)
                        <*> parseData) <|>
  parseTerm "mod_inst" (VIR.ModuleInstance
                        <$> identifier
                        <*> (comma *> identifier)
                        <*> (comma *> parseMap identifier parseExpr)
                        <*> parseData) <|>
  (rWord "skip" *> return (VIR.Skip ()))
  where
    parseAsn k t = parseTerm k $
                   VIR.Assignment t
                   <$> parseExpr
                   <*> (comma *> parseExpr)
                   <*> parseData

parseEvent :: Parser (VIR.Event ())
parseEvent =
  parseTerm "posedge" (VIR.PosEdge <$> parseExpr <*> parseData) <|>
  parseTerm "negedge" (VIR.NegEdge <$> parseExpr <*> parseData) <|>
  (rWord "star" *> return (VIR.Star ()))

parseAlwaysBlock :: Parser (VIR.AlwaysBlock ())
parseAlwaysBlock =
  parseTerm "always" (VIR.AlwaysBlock
                     <$> parseEvent
                     <*> (comma *> parseStmt)
                     <*> parseData)

parseData :: Parser ()
parseData = return ()

--------------------------------------------------------------------------------
-- | Tokenisers and Whitespace
--------------------------------------------------------------------------------

parseTerm :: String -> Parser a -> Parser a
parseTerm t p = rWord t *> parens p

parseMap :: (Eq k, Hashable k)
         => Parser k -> Parser v -> Parser (HM.HashMap k v)
parseMap kp vp = HM.fromList . toList <$> list ( parens ((,) <$> kp <*> (comma *> vp)))

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = spaceConsumer *> p <* MP.eof

spaceConsumer :: Parser ()
spaceConsumer = MPL.space (void MPC.spaceChar) lineCmnt blockCmnt
  where
    blockCmnt, lineCmnt :: Parser ()
    blockCmnt = MPL.skipBlockComment "/*" "*/"
    lineCmnt  = MPL.skipLineComment "%"

-- | `symbol s` parses just the string s (and trailing whitespace)
symbol :: String -> Parser Id
symbol s = T.pack <$> MPL.symbol spaceConsumer s

comma :: Parser Id
comma = symbol ","

-- lparen, rparen :: Parser Id
-- lparen = symbol "("
-- rparen = symbol ")"

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = betweenS "(" ")"

list :: Parser a -> Parser (L a)
list p = betweenS "[" "]" (sepBy comma p)

betweenS :: String -> String -> Parser a -> Parser a
betweenS l r = MP.between (symbol l) (symbol r)

-- | `lexeme p` consume whitespace after running p
lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme spaceConsumer

-- | `rWord`
rWord   :: String -> Parser Id
rWord w = (T.pack <$> MPC.string w) <* MP.notFollowedBy MPC.alphaNumChar <* spaceConsumer

many :: Parser a -> Parser (L a)
many p = go id
  where
    go f = do
      r <- MP.optional p
      case r of
        Nothing -> return (f SQ.empty)
        Just  x -> go (f . (x SQ.<|))
{-# INLINE many #-}

sepBy :: Parser sep -> Parser a -> Parser (L a)
sepBy sep p = do
  r <- MP.optional p
  case r of
    Nothing -> return SQ.empty
    Just  x -> (x SQ.<|) <$> many (sep >> p)
{-# INLINE sepBy #-}

keywords :: [Id]
keywords =
  [ "register", "wire", "input", "output", "posedge", "negedge", "star", "always", "module" -- module
  , "const", "var", "uf", "ite_expr", "str", "select"                 -- expr
  , "block", "b_asn", "nb_asn", "asn", "ite_stmt", "skip", "mod_inst" -- stmt
  ]

identifier :: Parser Id
identifier = lexeme (p >>= check)
  where
    p :: Parser Id
    p = idCons <$> (MPC.letterChar <|> MPC.char '_') <*> (T.pack <$> MP.many nonFirstChar)

    nonFirstChar :: Parser Char
    nonFirstChar = MP.satisfy (\a -> isDigit a || isLetter a || a == '_')

    check x = if x `elem` keywords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

--------------------------------------------------------------------------------
-- | Error handling
--------------------------------------------------------------------------------

renderError :: IRParseError -> IO String
renderError = return . eMsg

newtype IRParseError = IRParseError {eMsg :: String}
                     deriving (Show, Typeable)

instance Exception IRParseError

newtype SP = SP MP.SourcePos
           deriving (Eq, Ord)

instance MP.ShowErrorComponent SP where
  showErrorComponent (SP pos) = "parse error in file " ++ MP.sourceName pos

myParseErrorPretty :: MP.Stream s => MP.ParseErrorBundle s e -> String
myParseErrorPretty (MP.ParseErrorBundle errs posSt) =
  MP.errorBundlePretty $
  MP.ParseErrorBundle
  ((\(e,pos) -> MP.mapParseError (const (SP pos)) e) <$> fst (MP.attachSourcePos MP.errorOffset errs posSt))
  posSt
