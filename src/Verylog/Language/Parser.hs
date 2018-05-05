{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Verylog.Language.Parser ( parse
                               , parseWithoutConversion
                               , renderError
                               , IRParseError (..)
                               ) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.State.Lazy
import           Data.Char (isLetter, isDigit)
import           Data.Hashable
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

data ParsePort = PInput  String
               | POutput String
               deriving (Eq, Show)

data ParseVar = PRegister String
              | PWire     String
              deriving (Eq, Show)

data ParseBehavior = PAlways ParseEvent ParseStmt
                   deriving (Show)

data ParseEvent = PStar
                | PPosEdge String
                | PNegEdge String
                deriving (Show)

data ParseUF = PUF String [String]
               deriving (Show)

data ParseGate = PContAsgn String String
               | PModuleInst { pmModuleName    :: String
                             , pmInstName      :: String            -- name of the module
                             , pmInstPorts     :: [ParsePort]       -- port list (i.e. formal parameters)
                             , pmInstVars      :: [ParseVar]        -- wires or registers used
                             , pmInstGates     :: [ParseGate]       -- assign or module instantiations
                             , pmInstBehaviors :: [ParseBehavior]   -- always blocks
                             , pmInstUFs       :: [ParseUF]         -- uninterpreted functions
                             }
               deriving (Show)
                         
data ParseIR = TopModule    { mPortNames :: [ParsePort]       -- port list (i.e. formal parameters)
                            , mPorts     :: [ParseVar]        -- wires os registers used
                            , mGates     :: [ParseGate]       -- assign or module instantiations
                            , mBehaviors :: [ParseBehavior]   -- always blocks
                            , mUFs       :: [ParseUF]         -- uninterpreted functions
                            }
             | PSource      String
             | PSink        String
             | PSanitize    [String]
             | PNotSanitize { nsPortName   :: String
                            , nsModuleName :: Maybe String
                            }
             | PSanitizeMod { sModuleName :: String
                            , sVarName    :: String
                            }
             | PSanitizeGlob String
             | PTaintEq      String
             deriving (Show)

data ParseStmt = PBlock           [ParseStmt]
               | PBlockingAsgn    String
                                  String
               | PNonBlockingAsgn String
                                  String
               | PIfStmt          String
                                  ParseStmt
                                  ParseStmt
               | PSkip
               deriving (Show)

data ParseSt = ParseSt { _parseSources      :: S.HashSet Id
                       , _parseSinks        :: S.HashSet Id
                       , _parsePorts        :: S.HashSet Var
                       , _parseSanitize     :: S.HashSet Id
                       , _parseNotSanitize  :: M.HashMap Id (S.HashSet Id)
                       , _parseSanitizeGlob :: S.HashSet Id
                       , _parseTaintEq      :: S.HashSet Id
                       , _parseModSanitize  :: M.HashMap Id (S.HashSet Id)
                       , _parseUFs          :: M.HashMap Id [Id]
                       , _st                :: ! St
                       }

emptyParseSt :: ParseSt
emptyParseSt = ParseSt { _parseSources      = S.empty
                       , _parseSinks        = S.empty
                       , _parsePorts        = S.empty
                       , _parseSanitize     = S.empty
                       , _parseNotSanitize  = M.empty
                       , _parseSanitizeGlob = S.empty
                       , _parseTaintEq      = S.empty
                       , _parseModSanitize  = M.empty
                       , _parseUFs          = M.empty
                       , _st                = emptySt
                       }

makeLenses ''ParseSt

-----------------------------------------------------------------------------------
-- | ParseIR -> St
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
makeState :: [ParseIR] -> St
-----------------------------------------------------------------------------------
makeState (topIR@(TopModule{..}):annots) = resultState -- trace (show (resultState^.sanitize)) resultState
  where
    resultState = evalState comp emptyParseSt
  
    flattenUFs   :: M.HashMap Id [Id] -> M.HashMap Id [Id]
    flattenUFs m = let varDeps v = case M.lookup v m of
                                     Nothing -> [v]
                                     Just as -> concatMap varDeps as
                   in M.mapWithKey (\k _ -> varDeps k) m

    loc = ("TOPLEVEL", "TOPLEVEL")

    comp = do
      -- 1. collect taint information (update the state's variables)
      sequence_ $ collectTaint <$> (annots ++ [topIR])

      -- 2. remove sanitizes
      sanitizes     <- use parseSanitize
      maybeNegVars  <- uses parseNotSanitize (M.lookup "")
      let negVars   = case maybeNegVars of
                        Nothing -> S.empty
                        Just s  -> s
      parseSanitize .= sanitizes `S.difference` negVars

      -- 3. collect ports and ufs
      collectNonTaint topIR

      -- 4. update taint info of st
      st . ports        <~ uses parsePorts        S.toList
      st . ufs          <~ uses parseUFs          flattenUFs
      st . sources      <~ uses parseSources      S.toList
      st . sinks        <~ uses parseSinks        S.toList
      st . taintEq      <~ uses parseTaintEq      S.toList
      st . sanitize     <~ uses parseSanitize     S.toList
      st . sanitizeGlob <~ uses parseSanitizeGlob S.toList

      -- 5. create intermediary IR from parse IR
      st . irs <~ uses st (makeIntermediaryIR loc mBehaviors mGates)

      -- 6. do some sanity checks
      sanityChecks

      use st

makeState _ = throw (PassError "First ir is not a toplevel module !")

sanityChecks :: State ParseSt ()
sanityChecks = do
  -- make sure we have at least one source and a sink
  let isEmpty = (== 0) . length
  noTaint <- liftM2 (||) (uses (st.sinks) isEmpty) (uses (st.sources) isEmpty)
  when noTaint $
    throw (PassError "Source or sink taint information is missing")

  -- check if source and sink variables actually exist
  varNames   <- uses (st . ports) (S.fromList . (map varName))
  allSources <- use parseSources
  allSinks   <- use parseSinks

  let isTaintInvalid s = not $ S.null $ S.difference s varNames

  when (isTaintInvalid allSources || isTaintInvalid allSinks) $
    error $
    printf "Source or sink taint variable is invalid\n  vars: %s\n  sources: %s\n  sinks: %s\n"
    (show varNames) (show allSources) (show allSinks)

  -- make sure source or sink variables are registers
  rs   <- uses (st . ports) (map varName . filter isRegister)
  srcs <- use (st . sources)
  snks <- use (st . sinks)

  let src_dif = srcs Li.\\ rs
  let snk_dif = snks Li.\\ rs

  when (src_dif /= [] || snk_dif /= []) $
    error $
    printf "Taint variable is not a register !\n  sources: %s\n  sinks: %s\n"
    (show src_dif) (show snk_dif)


-----------------------------------------------------------------------------------
collectTaint :: ParseIR -> State ParseSt ()
-----------------------------------------------------------------------------------
collectTaint (PSource s)        = parseSources      %= S.insert s
collectTaint (PSink s)          = parseSinks        %= S.insert s
collectTaint (PTaintEq s)       = parseTaintEq      %= S.insert s
collectTaint (PSanitize s)      = parseSanitize     %= S.union (S.fromList s)
collectTaint (PNotSanitize{..}) = case nsModuleName of
                                    Nothing -> parseNotSanitize %= mapOfSetInsert "" nsPortName
                                    Just m  -> parseNotSanitize %= mapOfSetInsert m  nsPortName
collectTaint (PSanitizeGlob s)  = do parseSanitizeGlob %= S.insert s
                                     parseSanitize     %= S.insert s
collectTaint (PSanitizeMod{..}) = parseModSanitize %= mapOfSetInsert sModuleName sVarName
collectTaint (TopModule{..})    = do sanitizeWires      mPorts
                                     sanitizeSubmodules mGates

-- wires are sanitized automatically
sanitizeWires      :: [ParseVar] -> State ParseSt ()
sanitizeWires vars = sequence_ $ sanitizeWire <$> vars
  where
    sanitizeWire               :: ParseVar -> State ParseSt ()
    sanitizeWire (PWire s)     = parseSanitize %= S.insert s
    sanitizeWire (PRegister _) = return ()

-- figures out which variables to sanitize inside the module instantiations
sanitizeSubmodules       :: [ParseGate] -> State ParseSt ()
sanitizeSubmodules gates = sequence_ $ sanitizeInst <$> gates
  where
    sanitizeInst                   :: ParseGate -> State ParseSt ()
    sanitizeInst (PContAsgn _ _)   = return ()
    sanitizeInst (PModuleInst{..}) = do
      sanitizeWires      pmInstVars
      sanitizeSubmodules pmInstGates

      vars         <- uses parseModSanitize (M.lookup pmModuleName)
      maybeNegVars <- uses parseNotSanitize (M.lookup pmModuleName)
      let negVars = case maybeNegVars of
                      Nothing -> S.empty
                      Just s  -> s
      case vars of
        Nothing -> return ()
        Just vs -> sequence_ $
                   (\v -> parseSanitize %= S.insert (mk_mod_var pmInstName v))
                   <$> S.toList (vs `S.difference` negVars)
      sanitizeSubmodules pmInstGates

    mk_mod_var m v = printf "%s_%s" m v


-----------------------------------------------------------------------------------
collectNonTaint :: ParseIR -> State ParseSt ()
-----------------------------------------------------------------------------------
collectNonTaint (TopModule{..}) = collectPortAndUFs mPorts mGates mUFs
collectNonTaint _               = error "collectNonTaint is called without top module"

-- collect ports and ufs
collectPortAndUFs :: [ParseVar] -> [ParseGate] -> [ParseUF] -> State ParseSt ()
collectPortAndUFs vs gs us = do
  sequence_ $ (\v          -> parsePorts %= S.insert (toVar v)) <$> vs
  sequence_ $ (\(PUF u as) -> parseUFs   %= M.insert u as)      <$> us
  sequence_ $ handleGate                                        <$> gs
  where
    handleGate (PModuleInst{..}) = collectPortAndUFs pmInstVars pmInstGates pmInstUFs 
    handleGate (PContAsgn _ _ )  = return ()

    toVar (PWire w)     = Wire w
    toVar (PRegister r) = Register r


-----------------------------------------------------------------------------------
makeIntermediaryIR :: Loc -> [ParseBehavior] -> [ParseGate] -> St -> [IR]
-----------------------------------------------------------------------------------
type Loc = (String, String)

makeIntermediaryIR loc alwaysBlocks gates topSt =
  (always2IR loc <$> alwaysBlocks) ++ (gate2IR loc topSt <$> gates)

gate2IR                           :: Loc -> St -> ParseGate -> IR
gate2IR loc _ (PContAsgn l r)     = Always Star (BlockingAsgn l r) loc
gate2IR _ topSt (PModuleInst{..}) = 
  ModuleInst{ modInstName = pmInstName
            , modParams   = toPort <$> pmInstPorts
            , modInstSt   = set irs (makeIntermediaryIR loc' pmInstBehaviors pmInstGates topSt) topSt
            }
  where
    toPort (PInput x)  = Input x
    toPort (POutput x) = Output x
    loc' = (pmModuleName, pmInstName)

always2IR                       :: Loc -> ParseBehavior -> IR
always2IR loc (PAlways ev stmt) = Always (makeEvent ev) (makeStmt stmt) loc

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

-- --------------------------------------------------------------------------------
-- | PARSING
-- --------------------------------------------------------------------------------
        
type Parser = Parsec SourcePos String

-- --------------------------------------------------------------------------------
parseWithoutConversion :: FilePath -> String -> [ParseIR]
-- --------------------------------------------------------------------------------
parseWithoutConversion f = parseWith parseIR f

-- --------------------------------------------------------------------------------
parse :: FilePath -> String -> St
-- --------------------------------------------------------------------------------
parse f = parseWithoutConversion f >>> makeState

parseWith  :: Parser a -> FilePath -> String -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> throw (IRParseError (parseErrorPretty err) (NE.head . errorPos $ err))
                    Right e  -> e

--------------------------------------------------------------------------------
-- | Top-Level Expression Parser
--------------------------------------------------------------------------------

parsePort :: Parser ParsePort
parsePort =     rWord "input"  *> parens (PInput  <$> identifier)
            <|> rWord "output" *> parens (POutput <$> identifier)

parseVar :: Parser ParseVar
parseVar = rWord "register" *> parens (PRegister <$> identifier)
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
                              <$> identifier            -- module name
                              <*> (comma *> identifier) -- instantiation name
                              <*> (comma *> list parsePort)
                              <*> (comma *> list parseVar)
                              <*> (comma *> list parseGate)
                              <*> (comma *> list parseBehavior)
                              <*> (comma *> list parseUF))

parseTopModule :: Parser ParseIR
parseTopModule = spaceConsumer
                 *> rWord "topmodule"
                 *> parens (TopModule
                             <$> list parsePort
                             <*> (comma *> list parseVar)
                             <*> (comma *> list parseGate)
                             <*> (comma *> list parseBehavior)
                             <*> (comma *> list parseUF))
                 <* char '.' <* spaceConsumer

parseTaint :: Parser ParseIR  
parseTaint = spaceConsumer
             *> (     rWord "taint_source"  *> parens (PSource <$> identifier)
                  <|> rWord "taint_sink"    *> parens (PSink <$> identifier)
                  <|> rWord "taint_eq"      *> parens (PTaintEq <$> identifier)
                  <|> rWord "sanitize_mod"  *> parens (PSanitizeMod <$> identifier <*> (comma *> identifier))
                  <|> rWord "sanitize_glob" *> parens (PSanitizeGlob <$> identifier)
                  <|> rWord "not_sanitize"  *> parens (PNotSanitize <$> identifier <*> optionMaybe (comma *> identifier))
                  <|> rWord "sanitize"      *> parens (PSanitize <$> parseMany1 identifier comma)
                )
             <* char '.' <* spaceConsumer

parseStmt :: Parser ParseStmt  
parseStmt =     rWord "block"  *> parens (PBlock           <$> list parseStmt)
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
spaceConsumer = (L.space (void spaceChar) lineCmnt blockCmnt) -- *> (L.space (void spaceChar) prologDecl blockCmnt)
  where blockCmnt    = L.skipBlockComment "/*" "*/"
        lineCmnt     = L.skipLineComment "%"
        -- prologDecl   = L.skipLineComment ":-"

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

-- parseSep :: Parser a -> 

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
  , "sanitize", "sanitize_mod"
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

parseMany1 :: Parser a -> Parser b -> Parser [a]
parseMany1 elemP sepP = 
    (:) <$> elemP <*> many (sepP *> elemP)

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

instance Hashable ParseVar where
  hashWithSalt n (PRegister s) = hashWithSalt n ("parse-register", s)
  hashWithSalt n (PWire s)     = hashWithSalt n ("parse-wire", s)


optionMaybe   :: Parser a -> Parser (Maybe a)
optionMaybe p = Just <$> p <|> return Nothing
