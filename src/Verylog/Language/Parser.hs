{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Verylog.Language.Parser ( parse
                               , renderError
                               , IRParseError (..)
                               , ParseInput
                               , ParseOutput
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
import           Data.Typeable
import           Text.Megaparsec            as MP hiding (parse, State(..))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

import           Verylog.Language.Types
import           Verylog.Language.Utils
import           Verylog.Solver.FP.Types

-----------------------------------------------------------------------------------
-- | Verylog IR
-----------------------------------------------------------------------------------

data ParsePort = PInput  ! String
               | POutput ! String
               deriving (Eq, Show)

data ParseVar = PRegister ! String
              | PWire     ! String
              deriving (Eq, Show)

data ParseBehavior = PAlways ! ParseEvent ! ParseStmt
                   deriving (Show)

data ParseEvent = PStar
                | PPosEdge ! String
                | PNegEdge ! String
                deriving (Show)

data ParseUF = PUF  String [String]
             | PUF2 { ufVarName  :: ! String
                    , ufFuncName :: ! String
                    , ufArgs     :: ! [String]
                    }
               deriving (Show)

data ParseGate = PContAsgn String String
               | PModuleInst { pmModuleName    :: ! String
                             , pmInstName      :: ! String            -- name of the module
                             , pmInstPorts     :: ! [ParsePort]       -- port list (i.e. formal parameters)
                             , pmInstVars      :: ! [ParseVar]        -- wires or registers used
                             , pmInstGates     :: ! [ParseGate]       -- assign or module instantiations
                             , pmInstBehaviors :: ! [ParseBehavior]   -- always blocks
                             , pmInstUFs       :: ! [ParseUF]         -- uninterpreted functions
                             }
               deriving (Show)

data ParseIR = TopModule    { mPortNames :: ! [ParsePort]       -- port list (i.e. formal parameters)
                            , mPorts     :: ! [ParseVar]        -- wires os registers used
                            , mGates     :: ! [ParseGate]       -- assign or module instantiations
                            , mBehaviors :: ! [ParseBehavior]   -- always blocks
                            , mUFs       :: ! [ParseUF]         -- uninterpreted functions
                            }
             | PAnnotation  Annotation
             | PQualifier   FPQualifier
             deriving (Show)

data ParseStmt = PBlock           ! [ParseStmt]
               | PBlockingAsgn    ! String
                                  ! String
               | PNonBlockingAsgn ! String
                                  ! String
               | PIfStmt          ! String
                                  ! ParseStmt
                                  ! ParseStmt
               | PSkip
               deriving (Show)

data ParseSt = ParseSt { _parseSources      :: ! (S.HashSet Id)
                       , _parseSinks        :: ! (S.HashSet Id)
                       , _parsePorts        :: ! (S.HashSet Var)
                       , _parseSanitize     :: ! (S.HashSet Id)
                       , _parseSanitizeGlob :: ! (S.HashSet Id)
                       , _parseTaintEq      :: ! (S.HashSet Id)
                       , _parseAssertEq     :: ! (S.HashSet Id)
                       , _parseModSanitize  :: ! (M.HashMap Id (S.HashSet Id))
                       , _parseUFs          :: ! UFMap
                       , _st                :: ! St
                       , _annots            :: ! AnnotSt
                       }

emptyParseSt :: ParseSt
emptyParseSt = ParseSt { _parseSources      = mempty
                       , _parseSinks        = mempty
                       , _parsePorts        = mempty
                       , _parseSanitize     = mempty
                       , _parseSanitizeGlob = mempty
                       , _parseTaintEq      = mempty
                       , _parseAssertEq     = mempty
                       , _parseModSanitize  = mempty
                       , _parseUFs          = mempty
                       , _st                = mempty
                       , _annots            = mempty
                       }

makeLenses ''ParseSt

type Parser = Parsec SourcePos String

type ParseInput  = (FilePath, String)
type ParseOutput = (St, AllAnnots)

-- --------------------------------------------------------------------------------
parse :: ParseInput -> ParseOutput
-- --------------------------------------------------------------------------------
parse = parseWithoutConversion >>> first makeState

parseWithoutConversion :: ParseInput -> ([ParseIR], AllAnnots)
parseWithoutConversion (fp, s) = foldr f ([],mempty) (parseWith parseIR)
  where
    f (PQualifier q)    = second $ over allQualifiers ((:) q)
    f p@(PAnnotation a) = first ((:) p) >>> second (over allAnnotations ((:) a))
    f p                 = first ((:) p)

    parseWith p =
      case runParser (whole p) fp s of
        Right e     -> e
        Left bundle -> throw (IRParseError (myParseErrorPretty bundle))

-----------------------------------------------------------------------------------
makeState :: [ParseIR] -> St
-----------------------------------------------------------------------------------
makeState (topIR@(TopModule{..}):as) = resultState -- trace (show (resultState^.sanitize)) resultState
  where
    resultState = evalState comp emptyParseSt

    flattenUFs   :: UFMap -> UFMap
    flattenUFs m = let varDeps :: Id -> [Id]
                       varDeps v = case M.lookup v m of
                                     Nothing        -> [v]
                                     Just (_, args) -> concatMap varDeps args
                   in M.mapWithKey (\k (f, _) -> (f, varDeps k)) m

    loc = ("TOPLEVEL", "TOPLEVEL")

    comp = do
      -- collect taint information (update the state's variables)
      sequence_ $ collectTaint <$> (as ++ [topIR])

      -- collect ports and ufs
      collectNonTaint topIR
      st . ports <~ uses parsePorts S.toList
      st . ufs   <~ uses parseUFs   flattenUFs

      -- update taint info of st
      annots . sources      <~ use parseSources
      annots . sinks        <~ use parseSinks
      annots . taintEq      <~ use parseTaintEq
      annots . assertEq     <~ use parseAssertEq
      annots . sanitize     <~ use parseSanitize
      annots . sanitizeGlob <~ use parseSanitizeGlob

      prts <- use (st . ports)
      let topWireInputs =
            let f (PInput i) l  = if   (Wire i) `elem` prts
                                  then i:l
                                  else l
                f (POutput _) l = l
            in  foldr f [] mPortNames

      -- top module's input wires are tainted eq
      annots . taintEq %= S.union (S.fromList topWireInputs)

      -- create intermediary IR from parse IR
      st . irs <~ uses st (makeIntermediaryIR loc mBehaviors mGates)

      -- do some sanity checks
      sanityChecks

      use st

makeState _ = throw (PassError "First ir is not a toplevel module !")

sanityChecks :: State ParseSt ()
sanityChecks = do
  -- make sure we have at least one source and a sink
  srcs <- uses parseSources S.toList
  snks <- uses parseSinks   S.toList
  when (null srcs || null snks) $
    throw (PassError "Source or sink taint information is missing")

  prts <- use parsePorts
  let f p = S.member (Register p) prts || S.member (Wire p) prts
  when (not $ all f (srcs ++ snks)) $
    throw (PassError "Source or sink taint is an unknown variable")

  taintEqs      <- use parseTaintEq
  assertEqs     <- use parseAssertEq
  sanitizes     <- use parseSanitize
  sanitizeGlobs <- use parseSanitizeGlob
  let allOtherAnnots = taintEqs `S.union` assertEqs `S.union` sanitizes `S.union` sanitizeGlobs
      allOtherAnnotsDiff = allOtherAnnots `S.difference` (S.map varName prts)
  when (allOtherAnnotsDiff /= S.empty) $
    throw (PassError $ printf "Annotation has an unknown variable: %s" (show allOtherAnnotsDiff))

  -- check if source and sink variables actually exist, and
  -- make sure source or sink variables are registers
  rs  <- uses (st . ports) (map varName . filter isRegister)
  -- let src_dif = srcs Li.\\ rs
  -- when (src_dif /= [] || snk_dif /= []) $
  let snk_dif = snks Li.\\ rs
  when (snk_dif /= []) $
    error $
    printf "Taint variable is not a register !\n  sinks: %s\n"
    (show snk_dif)


-----------------------------------------------------------------------------------
collectTaint :: ParseIR -> State ParseSt ()
-----------------------------------------------------------------------------------
collectTaint (TopModule{..}) = sanitizeSubmodules mGates
collectTaint (PQualifier _) = return ()
collectTaint (PAnnotation annot)  = fromAnnot annot
  where
    fromAnnot :: Annotation -> State ParseSt ()
    fromAnnot (Source s)        = parseSources      %= S.insert s
    fromAnnot (Sink s)          = parseSinks        %= S.insert s
    fromAnnot (TaintEq s)       = parseTaintEq      %= S.insert s
    fromAnnot (AssertEq s)      = parseAssertEq     %= S.insert s
    fromAnnot (Sanitize s)      = parseSanitize     %= S.union (S.fromList s)
    fromAnnot (SanitizeGlob s)  = do parseSanitizeGlob %= S.insert s
                                     parseSanitize     %= S.insert s
    fromAnnot (SanitizeMod{..}) = parseModSanitize %= mapOfSetInsert annotModuleName annotVarName

-- figures out which variables to sanitize inside the module instantiations
sanitizeSubmodules       :: [ParseGate] -> State ParseSt ()
sanitizeSubmodules gates = sequence_ $ sanitizeInst <$> gates
  where
    sanitizeInst                   :: ParseGate -> State ParseSt ()
    sanitizeInst (PContAsgn _ _)   = return ()
    sanitizeInst (PModuleInst{..}) = do
      sanitizeSubmodules pmInstGates

      vs <- uses parseModSanitize (M.lookup pmModuleName)
      case vs of
        Nothing  -> return ()
        Just vs2 -> sequence_ $
                    (\v -> parseSanitize %= S.insert (mk_mod_var pmInstName v))
                    <$> S.toList vs2
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
  sequence_ $ (\v -> parsePorts %= S.insert (toVar v)) <$> vs
  sequence_ $
    (\case
        PUF u as -> parseUFs %= M.insert u (u, as)
        PUF2{..} -> parseUFs %= M.insert ufVarName (ufFuncName, ufArgs)
    )      <$> us
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
gate2IR loc _ (PContAsgn l r)     = Always Assign (BlockingAsgn l r) loc
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
parseUF =
      rWord "linkF" *> parens (PUF2
                               <$> identifier
                               <*> (comma *> identifier)
                               <*> (comma *> list identifier))
  <|> rWord "link" *> parens (PUF
                              <$> identifier
                              <*> (comma *> list identifier))

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
             *> ( PAnnotation <$> parseAnnot <|>
                  PQualifier <$> parseQual
                )
             <* char '.' <* spaceConsumer
  where
    parseAnnot =     rWord "taint_source"  *> parens (Source <$> identifier)
                 <|> rWord "taint_sink"    *> parens (Sink <$> identifier)
                 <|> rWord "taint_eq"      *> parens (TaintEq <$> identifier)
                 <|> rWord "assert_eq"     *> parens (AssertEq <$> identifier)
                 <|> rWord "sanitize_mod"  *> parens (SanitizeMod <$> identifier <*> (comma *> identifier))
                 <|> rWord "sanitize_glob" *> parens (SanitizeGlob <$> identifier)
                 <|> rWord "sanitize"      *> parens (Sanitize <$> parseMany1 identifier comma)

    parseQual =     rWord "qualifierPairs"  *> parens (QualifPairs  <$> list identifier)
                <|> rWord "qualifierAssume" *> parens (QualifAssume <$> list identifier)
                <|> rWord "qualifierIff"    *> parens (QualifIff    <$> identifier <*> (comma *> list identifier))
                <|> rWord "qualifierImp"    *> parens (QualifImp    <$> identifier <*> (comma *> list identifier))

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
spaceConsumer = (L.space (void spaceChar) lineCmnt blockCmnt)
  where
    blockCmnt, lineCmnt :: Parser ()
    blockCmnt = L.skipBlockComment "/*" "*/"
    lineCmnt  = L.skipLineComment "%"

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
    p :: Parser String
    p = (:) <$> letterChar <*> many nonFirstChar

    nonFirstChar :: Parser Char
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

    check x = if x `elem` keywords
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
renderError = return . eMsg

data IRParseError = IRParseError { eMsg :: !String
                                 }
                  deriving (Show, Typeable)

instance Exception IRParseError

instance Hashable ParseVar where
  hashWithSalt n (PRegister s) = hashWithSalt n ("parse-register", s)
  hashWithSalt n (PWire s)     = hashWithSalt n ("parse-wire", s)


optionMaybe   :: Parser a -> Parser (Maybe a)
optionMaybe p = Just <$> p <|> return Nothing

myParseErrorPretty :: (Stream s) => ParseErrorBundle s e -> String
myParseErrorPretty (ParseErrorBundle errs posSt) =
  errorBundlePretty $
  ParseErrorBundle
  ((\(e,pos) -> mapParseError (const (SP pos)) e) <$> (fst $ attachSourcePos errorOffset errs posSt))
  posSt

newtype SP = SP SourcePos
           deriving (Eq, Ord)

instance ShowErrorComponent SP where
  showErrorComponent (SP pos) = "parse error in file " ++ (MP.sourceName pos)
