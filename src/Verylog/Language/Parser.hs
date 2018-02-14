{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Verylog.Language.Parser ( parse
                               , renderError
                               , IRParseError (..)
                               ) where

import           Control.Exception
import           Control.Monad (void)
import           Data.Char (isLetter, isDigit)
import qualified Data.List                  as Li
import           Data.List.NonEmpty         as NE
import           Data.Typeable
import           Text.Megaparsec            as M hiding (parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

import           Verylog.Language.Types
import           Verylog.Language.Utils

type Parser = Parsec SourcePos String

-- --------------------------------------------------------------------------------
parse :: FilePath -> String -> [IR]
-- --------------------------------------------------------------------------------
parse = parseWith $ many parseIR

parseWith  :: Parser a -> FilePath -> String -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> throw (IRParseError (parseErrorPretty err) (NE.head . errorPos $ err))
                    Right e  -> e

--------------------------------------------------------------------------------
-- | Top-Level Expression Parser
--------------------------------------------------------------------------------

parseIR :: Parser IR
parseIR = spaceConsumer *> _parseIR <* char '.' <* spaceConsumer

_parseIR :: Parser IR  
_parseIR = rWord "register"         *> parens (Register    <$> identifier)
           <|> rWord "wire"         *> parens (Wire        <$> identifier)
           <|> rWord "link"         *> parens (UF          <$> identifier <*> (comma *> list identifier))
           <|> rWord "always"       *> parens (Always      <$> parseEvent <*> (comma *> parseStmt))
           <|> rWord "asn"          *> parens (ContAsgn    <$> identifier <*> (comma *> identifier))
           <|> rWord "taint_source" *> parens (TaintSource <$> identifier)
           <|> rWord "taint_sink"   *> parens (TaintSink   <$> identifier)

parseStmt :: Parser Stmt  
parseStmt = rWord "block"      *> parens (Block        <$> list parseStmt)
            <|> rWord "b_asn"  *> parens (BlockingAsgn <$> identifier <*> (comma *> identifier))
            <|> rWord "nb_asn" *> parens (BlockingAsgn <$> identifier <*> (comma *> identifier))
            <|> rWord "ite"    *> parens (If <$> identifier <*> (comma *> parseStmt) <*> (comma *> parseStmt))
            <|> rWord "skip"   *> return Skip

parseEvent :: Parser Event
parseEvent = rWord "event1(star)" *> return Star
             <|> rWord "event2(posedge" *> comma *> (PosEdge <$> identifier) <* rWord ")"
             <|> rWord "event2(negedge" *> comma *> (PosEdge <$> identifier) <* rWord ")"

--------------------------------------------------------------------------------
-- | Tokenisers and Whitespace
--------------------------------------------------------------------------------

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = spaceConsumer *> p <* eof

spaceConsumer :: Parser ()
spaceConsumer = (L.space (void spaceChar) lineCmnt blockCmnt) *> (L.space (void spaceChar) prologDecl blockCmnt)
  where blockCmnt    = L.skipBlockComment "/*" "*/"
        prologDecl   = L.skipLineComment ":-"
        lineCmnt     = L.skipLineComment "%"

-- | `symbol s` parses just the string s (and trailing whitespace)
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

comma :: Parser String
comma = symbol ","

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = betweenS "(" ")"

list :: Parser a -> Parser [a]
list p = betweenS "[" "]" lp
  where
    lp = (:) <$> p <*> many (comma *> p)
         <|> return []

betweenS :: String -> String -> Parser a -> Parser a
betweenS l r = between (symbol l) (symbol r)

-- | `lexeme p` consume whitespace after running p
lexeme :: Parser a -> Parser a
lexeme p = L.lexeme spaceConsumer p

-- -- | 'integer' parses an integer.
-- integer :: Parser Integer
-- integer = lexeme L.decimal

-- | `rWord`
rWord   :: String -> Parser String
rWord w = string w <* notFollowedBy alphaNumChar <* spaceConsumer


-- | list of reserved words
keywords :: [String]
keywords =
  [ "register", "wire", "always", "link", "asn", "taint_source", "taint_sink"
  , "block", "b_asn", "nb_asn", "ite", "skip"
  ]

-- | `identifier` parses identifiers: lower-case alphabets followed by alphas or digits
identifier :: Parser String
identifier = lexeme (p >>= check)
  where
    p            = (:) <$> letterChar <*> many nonFirstChar
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
    check x      = if x `elem` keywords
                   then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                   else return x

-- -----------------------------------------------------------------------------
-- PRINTING ERROR MESSAGES -----------------------------------------------------
-- -----------------------------------------------------------------------------

readFilePos    :: SourcePos -> IO String
readFilePos pos = getPos pos <$> readFile (M.sourceName pos)

getPos :: SourcePos -> String -> String
getPos pos = getSpanSingle (unPos $ sourceLine pos) (unPos $ sourceColumn pos)

getSpanSingle :: Int -> Int -> String -> String
getSpanSingle l c
  = highlight l c 
  . safeHead ""
  . getRange l l
  . lines

highlight :: Int -> Int -> String -> String
highlight l c s = unlines
  [ cursorLine l s
  , replicate (12 + c) ' ' ++ "^"
  ]

cursorLine :: Int -> String -> String
cursorLine l s = printf "%s|  %s" (lineString l) s

lineString :: Int -> String
lineString n = replicate (10 - nD) ' ' ++ nS
  where
    nS       = show n
    nD       = Li.length nS

data IRParseError = IRParseError
  { eMsg :: !String
  , ePos :: !SourcePos
  }
  deriving (Show, Typeable)

instance Exception IRParseError

renderError :: IRParseError -> IO String
renderError e = do
  let pos = ePos e
      msg = eMsg e
  snippet <- readFilePos pos
  return $ printf "%s%s" snippet msg


instance ShowErrorComponent SourcePos where
  showErrorComponent pos = "parse error in file " ++ (M.sourceName pos)
