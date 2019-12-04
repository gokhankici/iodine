{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Iodine.Language.IRParser (parse , ParsedIR) where

import           Iodine.Language.IR
import           Iodine.Types

import           Control.Monad (void)
import           Data.Char (isLetter, isDigit)
import           Data.Foldable (toList)
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import           Text.Megaparsec            ((<|>))
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Data.Sequence              as SQ
import           Polysemy
import           Polysemy.Error

type Parser = MP.Parsec MP.SourcePos String
type ParsedIR = L (Module ())

parse :: Member (Error IodineException) r => (FilePath, String) -> Sem r ParsedIR
parse (fp, s) = parseWith (many parseModule)
  where
    parseWith p =
      case MP.runParser (whole p) fp s of
        Right e     -> return e
        Left bundle -> throw (IE IRParser (myParseErrorPretty bundle))

--------------------------------------------------------------------------------
-- | IR Parser
--------------------------------------------------------------------------------

parseModule :: Parser (Module ())
parseModule =
  parseTerm "module" $
  Module
  <$> identifier
  <*> (comma *> list parsePort)
  <*> (comma *> list parseVariable)
  <*> (comma *> list parseStmt)
  <*> (comma *> list parseAlwaysBlock)
  <*> (comma *> list parseModuleInstance)
  <*> parseData

parsePort :: Parser Port
parsePort =
  parseTerm "input"  (Input   <$> parseVariable) <|>
  parseTerm "output" (Output  <$> parseVariable)

parseVariable :: Parser Variable
parseVariable =
  parseTerm "wire"     (Wire     <$> identifier) <|>
  parseTerm "register" (Register <$> identifier)

parseExpr :: Parser (Expr ())
parseExpr =
  parseTerm "const" (Constant <$> constVar <*> parseData) <|>
  parseTerm "var" (Variable <$> identifier <*> (comma *> identifier) <*> parseData) <|>
  parseTerm "uf" (UF <$> identifier <*> (comma *> list parseExpr) <*> parseData) <|>
  parseTerm "ite_expr" (IfExpr
                        <$> parseExpr
                        <*> (comma *> parseExpr)
                        <*> (comma *> parseExpr)
                        <*> parseData) <|>
  parseTerm "str" (Str <$> identifier <*> parseData) <|>
  parseTerm "select" (Select
                      <$> parseExpr
                      <*> (comma *> list parseExpr)
                      <*> parseData)
  where
    constVar :: Parser Id
    constVar = T.pack <$> MP.many MPC.alphaNumChar

parseStmt :: Parser (Stmt ())
parseStmt =
  parseTerm "block" (Block <$> list parseStmt <*> parseData) <|>
  parseAsn "b_asn" Blocking <|>
  parseAsn "nb_asn" NonBlocking <|>
  parseAsn "asn" Continuous <|>
  parseTerm "ite_stmt" (IfStmt
                        <$> parseExpr
                        <*> (comma *> parseStmt)
                        <*> (comma *> parseStmt)
                        <*> parseData) <|>
  (rWord "skip" *> return (Skip ()))
  where
    parseAsn k t = parseTerm k $
                   Assignment t
                   <$> parseExpr
                   <*> (comma *> parseExpr)
                   <*> parseData

parseModuleInstance :: Parser (ModuleInstance ())
parseModuleInstance =
  parseTerm "mod_inst" $
  ModuleInstance
  <$> identifier
  <*> (comma *> identifier)
  <*> (comma *> parseMap identifier parseExpr)
  <*> parseData


parseEvent :: Parser (Event ())
parseEvent =
  parseTerm "posedge" (PosEdge <$> parseExpr) <|>
  parseTerm "negedge" (NegEdge <$> parseExpr) <|>
  (rWord "star" *> return Star)

parseAlwaysBlock :: Parser (AlwaysBlock ())
parseAlwaysBlock =
  parseTerm "always" (AlwaysBlock
                     <$> parseEvent
                     <*> (comma *> parseStmt))

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
    p = T.cons <$> (MPC.letterChar <|> MPC.char '_') <*> (T.pack <$> MP.many nonFirstChar)

    nonFirstChar :: Parser Char
    nonFirstChar = MP.satisfy (\a -> isDigit a || isLetter a || a == '_')

    check x = if x `elem` keywords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

--------------------------------------------------------------------------------
-- | Error handling
--------------------------------------------------------------------------------

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
