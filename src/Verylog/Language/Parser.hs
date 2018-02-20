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

data ParsePort = PRegister { parsePortName :: String }
               | PWire     { parsePortName :: String }

data ParseBehavior = PAlways ParseEvent ParseStmt

data ParseEvent = PStar
                | PPosEdge String
                | PNegEdge String

data ParseUF = PUF String [String]

data ParseGate = PContAsgn String String
               | PModuleInst { pmInstName      :: String            -- name of the module
                             , pmInstPortNames :: [String]          -- port list (i.e. formal parameters)
                             , pmInstArgs      :: [String]          -- instantiations (i.e. actual parameters)
                             , pmInstPorts     :: [ParsePort]       -- wires or registers used
                             , pmInstGates     :: [ParseGate]       -- assign or module instantiations
                             , pmInstBehaviors :: [ParseBehavior]   -- always blocks
                             , pmInstUFs       :: [ParseUF]         -- uninterpreted functions
                             }
                         
data ParseIR = TopModule { mPortNames :: [String]          -- port list (i.e. formal parameters)
                         , mPorts     :: [ParsePort]       -- wires os registers used
                         , mGates     :: [ParseGate]       -- assign or module instantiations
                         , mBehaviors :: [ParseBehavior]   -- always blocks
                         , mUFs       :: [ParseUF]         -- uninterpreted functions
                         }
             | PSource   String
             | PSink     String
             
data ParseStmt = PBlock           [ParseStmt]
               | PBlockingAsgn    String
                                  String
               | PNonBlockingAsgn String
                                  String
               | PIfStmt          String
                                  ParseStmt
                                  ParseStmt
               | PSkip

data ParseSt = ParseSt { _parseSources :: S.HashSet Id
                       , _parseSinks   :: S.HashSet Id
                       , _parsePorts   :: S.HashSet Id
                       , _st           :: St
                       }

makeLenses ''ParseSt

type Parser = Parsec SourcePos String

-- --------------------------------------------------------------------------------
parse :: FilePath -> String -> St
-- --------------------------------------------------------------------------------
parse f s = makeState $ parseWith parseIR f s

parseWith  :: Parser a -> FilePath -> String -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> throw (IRParseError (parseErrorPretty err) (NE.head . errorPos $ err))
                    Right e  -> e

--------------------------------------------------------------------------------
-- | Top-Level Expression Parser
--------------------------------------------------------------------------------

parsePort :: Parser ParsePort
parsePort = rWord "register" *> parens (PRegister <$> identifier)
            <|> rWord "wire" *> parens (PWire     <$> identifier)

parseBehavior :: Parser ParseBehavior
parseBehavior = rWord "always" *> parens (PAlways <$> parseEvent <*> (comma *> parseStmt))

parseEvent :: Parser ParseEvent
parseEvent = rWord "event1(star)" *> return PStar
             <|> rWord "event2(posedge" *> comma *> (PPosEdge <$> identifier) <* rWord ")"
             <|> rWord "event2(negedge" *> comma *> (PNegEdge <$> identifier) <* rWord ")"

parseUF :: Parser ParseUF
parseUF = rWord "link" *> parens (PUF <$> identifier <*> (comma *> list identifier))

parseGate :: Parser ParseGate
parseGate = rWord "asn" *> parens (PContAsgn <$> identifier <*> (comma *> identifier))
            <|> parseModuleInst

parseModuleInst :: Parser ParseGate
parseModuleInst = rWord "module"
                  *> parens (PModuleInst
                              <$> identifier
                              <*> (comma *> list identifier)
                              <*> (comma *> list identifier)
                              <*> (comma *> list parsePort)
                              <*> (comma *> list parseGate)
                              <*> (comma *> list parseBehavior)
                              <*> (comma *> list parseUF))

parseTopModule :: Parser ParseIR
parseTopModule = spaceConsumer
                 *> rWord "topmodule"
                 *> parens (TopModule
                             <$> list identifier
                             <*> (comma *> list parsePort)
                             <*> (comma *> list parseGate)
                             <*> (comma *> list parseBehavior)
                             <*> (comma *> list parseUF))
                 <* char '.' <* spaceConsumer

parseTaint :: Parser ParseIR  
parseTaint = spaceConsumer
             *> ( rWord "taint_source" *> parens (PSource <$> identifier)
                  <|> rWord "taint_sink" *> parens (PSink <$> identifier)
                )
             <* char '.' <* spaceConsumer

parseStmt :: Parser ParseStmt  
parseStmt = rWord "block"      *> parens (PBlock           <$> list parseStmt)
            <|> rWord "b_asn"  *> parens (PBlockingAsgn    <$> identifier <*> (comma *> identifier))
            <|> rWord "nb_asn" *> parens (PNonBlockingAsgn <$> identifier <*> (comma *> identifier))
            <|> rWord "ite"    *> parens (PIfStmt          <$> identifier <*> (comma *> parseStmt) <*> (comma *> parseStmt))
            <|> rWord "skip"   *> return PSkip

parseIR :: Parser [ParseIR]
parseIR = (:) <$> parseTopModule <*> many parseTaint

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
  , "block", "b_asn", "nb_asn", "ite", "skip", "module", "topmodule"
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

emptyParseSt = ParseSt { _parseSources = S.empty
                       , _parseSinks   = S.empty
                       , _parsePorts   = S.empty
                       , _st           = emptySt
                       }

makeState :: [ParseIR] -> St
makeState (TopModule{..}:taints) = evalState comp emptyParseSt
  where
    comp = do sequence_ $ collectVar <$> taints
              st           .= collectModule mPorts mGates mBehaviors mUFs
              st . sources <~ uses parseSources   S.toList
              st . sinks   <~ uses parseSinks     S.toList

              let f = (== 0) . length
              noTaint <- liftM2 (||) (uses (st.sinks) f) (uses (st.sources) f)
              when noTaint $ throw (PassError "Source or sink taint information is missing")

              use st
makeState _ = throw (PassError "First ir is not a toplevel module !")

collectVar                :: ParseIR -> State ParseSt ()
collectVar (PSource s)     = parseSources %= S.insert s
collectVar (PSink s)       = parseSinks   %= S.insert s
collectVar (TopModule{..}) = return ()
    
collectModule :: [ParsePort] -> [ParseGate] -> [ParseBehavior] -> [ParseUF] -> St
collectModule prts gates bhvs ufs = evalState comp emptyParseSt
  where
    comp = do sequence_ $ (\p -> parsePorts %= S.insert (parsePortName p)) <$> prts
              sequence_ (collectGate <$> reverse gates)
              sequence_ (collectBhv  <$> reverse bhvs)
              sequence_ (collectUF   <$> reverse ufs)
              st . ports <~ uses parsePorts S.toList
              use st

collectGate :: ParseGate -> State ParseSt ()
collectGate (PContAsgn l r)   = st . irs %= (:) (ContAsgn l r)
collectGate (PModuleInst{..}) = st . irs %= (:) ModuleInst{ modInstName = pmInstName
                                                          , modInstArgs = zip pmInstPortNames pmInstArgs
                                                          , modInstSt   = st'
                                                          }
  where st' = collectModule pmInstPorts pmInstGates pmInstBehaviors pmInstUFs

collectBhv :: ParseBehavior -> State ParseSt ()
collectBhv (PAlways ev stmt) = st . irs %= (:) (Always (makeEvent ev) (makeStmt stmt))

makeEvent                :: ParseEvent -> Event
makeEvent PStar          = Star
makeEvent (PPosEdge clk) = PosEdge clk
makeEvent (PNegEdge clk) = NegEdge clk

makeStmt                        :: ParseStmt -> Stmt
makeStmt (PBlock ss)            = Block (makeStmt <$> ss)
makeStmt (PBlockingAsgn l r)    = BlockingAsgn l r
makeStmt (PNonBlockingAsgn l r) = NonBlockingAsgn l r
makeStmt (PIfStmt cond th el)   = IfStmt cond (makeStmt th) (makeStmt el)
makeStmt  PSkip                 = Skip

collectUF              :: ParseUF -> State ParseSt ()
collectUF (PUF v args) = st . ufs %= M.insert v args
      
