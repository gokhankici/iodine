{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Language.Parser ( parse
                               , renderError
                               , IRParseError (..)
                               ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.State.Lazy
import           Data.Char (isLetter, isDigit)
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S
import qualified Data.List                  as Li
import qualified Data.List.NonEmpty         as NE
import           Data.Typeable
import           Text.Megaparsec            as MP hiding (parse, State(..))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

import           Verylog.Language.Types
import           Verylog.Language.Utils

-----------------------------------------------------------------------------------
-- | Verylog IR
-----------------------------------------------------------------------------------
data ParseIR = PRegister String
             | PWire     String
             | PUF       String
                         [String]
                         
             | PAlways   ParseEvent
                         ParseStmt
                         
             | PContAsgn String
                         String
                         
             | PSource   String
             | PSink     String
             
data ParseEvent = PStar
                | PPosEdge String
                | PNegEdge String

data ParseStmt = PBlock           [ParseStmt]
               | PBlockingAsgn    String
                                  String
               | PNonBlockingAsgn String
                                  String
               | PIfStmt          String
                                  ParseStmt
                                  ParseStmt
               | PSkip

data ParseSt = ParseSt { _parseIRs       :: [ParseIR]
                       , _parseRegisters :: S.HashSet Id
                       , _parseWires     :: S.HashSet Id
                       , _parseSources   :: S.HashSet Id
                       , _parseSinks     :: S.HashSet Id

                       , _st             :: St
                       }

makeLenses ''ParseSt

type Parser = Parsec SourcePos String

-- --------------------------------------------------------------------------------
parse :: FilePath -> String -> St
-- --------------------------------------------------------------------------------
parse f s = makeState $ parseWith (many parseIR) f s

parseWith  :: Parser a -> FilePath -> String -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> throw (IRParseError (parseErrorPretty err) (NE.head . errorPos $ err))
                    Right e  -> e

--------------------------------------------------------------------------------
-- | Top-Level Expression Parser
--------------------------------------------------------------------------------

parseIR :: Parser ParseIR
parseIR = spaceConsumer *> _parseIR <* char '.' <* spaceConsumer

_parseIR :: Parser ParseIR  
_parseIR = rWord "register"         *> parens (PRegister    <$> identifier)
           <|> rWord "wire"         *> parens (PWire        <$> identifier)
           <|> rWord "link"         *> parens (PUF          <$> identifier <*> (comma *> list identifier))
           <|> rWord "always"       *> parens (PAlways      <$> parseEvent <*> (comma *> parseStmt))
           <|> rWord "asn"          *> parens (PContAsgn    <$> identifier <*> (comma *> identifier))
           <|> rWord "taint_source" *> parens (PSource      <$> identifier)
           <|> rWord "taint_sink"   *> parens (PSink        <$> identifier)

parseStmt :: Parser ParseStmt  
parseStmt = rWord "block"      *> parens (PBlock           <$> list parseStmt)
            <|> rWord "b_asn"  *> parens (PBlockingAsgn    <$> identifier <*> (comma *> identifier))
            <|> rWord "nb_asn" *> parens (PNonBlockingAsgn <$> identifier <*> (comma *> identifier))
            <|> rWord "ite"    *> parens (PIfStmt          <$> identifier <*> (comma *> parseStmt) <*> (comma *> parseStmt))
            <|> rWord "skip"   *> return PSkip

parseEvent :: Parser ParseEvent
parseEvent = rWord "event1(star)" *> return PStar
             <|> rWord "event2(posedge" *> comma *> (PPosEdge <$> identifier) <* rWord ")"
             <|> rWord "event2(negedge" *> comma *> (PNegEdge <$> identifier) <* rWord ")"

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

--------------------------------------------------------------------------------
-- | Printing Error Messages
--------------------------------------------------------------------------------

readFilePos    :: SourcePos -> IO String
readFilePos pos = getPos pos <$> readFile (MP.sourceName pos)

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

renderError :: IRParseError -> IO String
renderError e = do
  let pos = ePos e
      msg = eMsg e
  snippet <- readFilePos pos
  return $ printf "%s%s" snippet msg

instance ShowErrorComponent SourcePos where
  showErrorComponent pos = "parse error in file " ++ (MP.sourceName pos)

data IRParseError = IRParseError
  { eMsg :: !String
  , ePos :: !MP.SourcePos
  }
  deriving (Show, Typeable)

instance Exception IRParseError

-----------------------------------------------------------------------------------
-- | ParseIR -> St
-----------------------------------------------------------------------------------

makeState :: [ParseIR] -> St
makeState input = evalState pipeline initialParseSt
  where
    initialParseSt = ParseSt { _parseIRs       = input
                             , _parseRegisters = S.empty
                             , _parseWires     = S.empty
                             , _parseSources   = S.empty
                             , _parseSinks     = S.empty
                             , _st             = initialSt
                             }
    initialSt      = St { _registers = []
                        , _wires     = []
                        , _ufs       = M.empty
                        , _sources   = []
                        , _sinks     = []
                        , _irs       = []
                        }
    pipeline       = collectVars >> makeIR >> lastpass
    lastpass       = do st . registers <~ uses parseRegisters S.toList
                        st . wires     <~ uses parseWires     S.toList
                        st . sources   <~ uses parseSources   S.toList
                        st . sinks     <~ uses parseSinks     S.toList
                        use st

-- -----------------------------------------------------------------------------
-- 1. Collect vars
-- -----------------------------------------------------------------------------

collectVars :: State ParseSt ()
collectVars = use parseIRs >>= sequence_ . (map collectVar)

collectVar :: ParseIR -> State ParseSt ()
collectVar (PRegister id) = parseRegisters %= S.insert id
collectVar (PWire id)     = parseWires     %= S.insert id
collectVar (PUF id vars)  = st . ufs       %= M.insert id vars
collectVar (PSource s)    = parseSources   %= S.insert s
collectVar (PSink s)      = parseSinks     %= S.insert s
collectVar _              = return ()

-- -----------------------------------------------------------------------------
-- 3. Make IR elements
-- -----------------------------------------------------------------------------
makeIR :: State ParseSt ()
makeIR = (st . irs) <~ (uses parseIRs (foldr makeIRFold []))

makeIRFold                      :: ParseIR -> [IR] -> [IR]
makeIRFold (PAlways event stmt) = (:) $ Always (makeEvent event) (makeStmt stmt)
makeIRFold (PContAsgn l r)      = (:) $ ContAsgn l r
makeIRFold _                    = id

makeEvent :: ParseEvent -> Event
makeEvent PStar          = Star
makeEvent (PPosEdge clk) = PosEdge clk
makeEvent (PNegEdge clk) = NegEdge clk

makeStmt                       :: ParseStmt -> Stmt
makeStmt (PBlock ss)            = Block (makeStmt <$> ss)
makeStmt (PBlockingAsgn l r)    = BlockingAsgn l r
makeStmt (PNonBlockingAsgn l r) = NonBlockingAsgn l r
makeStmt (PIfStmt cond th el)   = IfStmt cond (makeStmt th) (makeStmt el)
makeStmt  PSkip                 = Skip
