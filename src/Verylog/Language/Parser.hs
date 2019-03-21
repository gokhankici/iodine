{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Verylog.Language.Parser ( parse
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
import qualified Data.Text                  as T
import           Data.Typeable
import           Text.Megaparsec            as MP hiding (parse, State(..))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf
import qualified Data.Sequence              as SQ

import Verylog.Utils
import Verylog.Language.Types
import Verylog.Solver.FP.Types

-----------------------------------------------------------------------------------
-- | Verylog IR
-----------------------------------------------------------------------------------

data ParsePort = PInput  Id
               | POutput Id
               deriving (Eq, Show)

data ParseVar = PRegister Id
              | PWire     Id
              deriving (Eq, Show)

data ParseBehavior = PAlways ParseEvent ParseStmt
                   deriving (Show)

data ParseEvent = PStar
                | PPosEdge Id
                | PNegEdge Id
                deriving (Show)

data ParseUF = PUF  Id [Id]
             | PUF2 { ufVarName  :: Id
                    , ufFuncName :: Id
                    , ufArgs     :: [Id]
                    }
               deriving (Show)

data ParseGate = PContAsgn Id Id
               | PModuleInst { pmModuleName    :: Id
                             , pmInstName      :: Id            -- name of the module
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
             | PAnnotation  Annotation
             | PQualifier   FPQualifier
             deriving (Show)

data ParseStmt = PBlock           [ParseStmt]
               | PBlockingAsgn    Id
                                  Id
               | PNonBlockingAsgn Id
                                  Id
               | PIfStmt          Id
                                  ParseStmt
                                  ParseStmt
               | PSkip
               deriving (Show)

data ParseSt = ParseSt { _parseSources      :: S.HashSet Id
                       , _parseSinks        :: S.HashSet Id
                       , _parsePorts        :: S.HashSet Var
                       , _parseSanitize     :: S.HashSet Id
                       , _parseSanitizeGlob :: S.HashSet Id
                       , _parseTaintEq      :: S.HashSet Id
                       , _parseAssertEq     :: S.HashSet Id
                       , _parseModSanitize  :: M.HashMap Id (S.HashSet Id)
                       , _parseUFs          :: M.HashMap Id (Id, [Id])
                       , _st                :: St
                       , _annots            :: AnnotSt
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
type ParseOutput = ((St, AnnotSt), [FPQualifier])

-- --------------------------------------------------------------------------------
parse :: ParseInput -> ParseOutput
-- --------------------------------------------------------------------------------
parse = parseWithoutConversion >>> first makeState

parseWithoutConversion :: ParseInput -> ([ParseIR], [FPQualifier])
parseWithoutConversion (fp, s) = foldr f ([],mempty) (parseWith parseIR)
  where
    -- separate extra qualifiers from the rest
    f (PQualifier q) = second $ ((:) q)
    f p              = first ((:) p)

    parseWith p =
      case runParser (whole p) fp s of
        Right e     -> e
        Left bundle -> throw (IRParseError (myParseErrorPretty bundle))

-----------------------------------------------------------------------------------
makeState :: [ParseIR] -> (St, AnnotSt)
-----------------------------------------------------------------------------------
makeState (topIR@(TopModule{..}):as) = resultState -- trace (show (resultState^.sanitize)) resultState
  where
    resultState = evalState comp emptyParseSt

    loc = ("TOPLEVEL", "TOPLEVEL") :: (Id, Id)

    comp = do
      -- collect taint information (update the state's variables)
      sequence_ $ collectTaint <$> (as ++ [topIR])

      -- collect ports and ufs
      collectNonTaint topIR
      st . ports <~ uses parsePorts set2seq

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
      st . irs <~ makeIntermediaryIR loc mBehaviors mGates

      -- do some sanity checks
      sanityChecks

      (,) <$> use st <*> use annots

makeState _ = throw (PassError "First ir is not a toplevel module !")

type P = State ParseSt

sanityChecks :: P ()
sanityChecks = do
  -- make sure we have at least one source and a sink
  srcs <- uses parseSources S.toList
  snks <- uses parseSinks   S.toList
  snkSet <- use parseSinks
  when (null srcs || null snks) $
    throw (PassError "Source or sink taint information is missing")

  prts <- use parsePorts
  let f p = S.member (Register p) prts || S.member (Wire p) prts
  forM_ (srcs ++ snks) $ \p ->
    if f p then return () else throw (PassError $ printf "Source or sink taint %s is an unknown variable" p)

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
  rs  <- uses (st . ports) (seq2set . fmap varName . SQ.filter isRegister)
  let snk_dif = snkSet `S.difference` rs
  when (not $ S.null snk_dif) $
    error $
    printf "Taint variable is not a register !\n  sinks: %s\n"
    (show snk_dif)


-----------------------------------------------------------------------------------
collectTaint :: ParseIR -> P ()
-----------------------------------------------------------------------------------
collectTaint (TopModule{..}) = sanitizeSubmodules mGates
collectTaint (PQualifier _) = return ()
collectTaint (PAnnotation annot)  = fromAnnot annot
  where
    fromAnnot :: Annotation -> P ()
    fromAnnot (Source s)        = parseSources      %= S.insert s
    fromAnnot (Sink s)          = parseSinks        %= S.insert s
    fromAnnot (TaintEq s)       = parseTaintEq      %= S.insert s
    fromAnnot (AssertEq s)      = parseAssertEq     %= S.insert s
    fromAnnot (Sanitize s)      = parseSanitize     %= S.union (S.fromList s)
    fromAnnot (SanitizeGlob s)  = parseSanitizeGlob %= S.insert s >>
                                  parseSanitize     %= S.insert s
    fromAnnot (SanitizeMod{..}) = parseModSanitize %= mapOfSetInsert annotModuleName annotVarName

-- figures out which variables to sanitize inside the module instantiations
sanitizeSubmodules       :: [ParseGate] -> P ()
sanitizeSubmodules gates = sequence_ $ sanitizeInst <$> gates
  where
    sanitizeInst :: ParseGate -> P ()
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

    mk_mod_var m v = m `idAppend` "_" `idAppend` v


-----------------------------------------------------------------------------------
collectNonTaint :: ParseIR -> P ()
-----------------------------------------------------------------------------------
collectNonTaint (TopModule{..}) = collectPortAndUFs mPorts mGates mUFs
collectNonTaint _               = error "collectNonTaint is called without top module"

-- collect ports and ufs
collectPortAndUFs :: [ParseVar] -> [ParseGate] -> [ParseUF] -> P ()
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


type Loc = (Id, Id)
-----------------------------------------------------------------------------------
makeIntermediaryIR :: Loc -> [ParseBehavior] -> [ParseGate] -> P (SQ.Seq IR)
-----------------------------------------------------------------------------------
makeIntermediaryIR loc alwaysBlocks gates = do
  irs1 <- mapM (always2IR loc) $ SQ.fromList alwaysBlocks
  irs2 <- mapM (gate2IR loc) $ SQ.fromList gates
  return $ irs1 SQ.>< irs2

always2IR :: Loc -> ParseBehavior -> P IR
always2IR loc (PAlways ev stmt) = do
  s <- makeStmt stmt
  return $ Always (makeEvent ev) s loc

makeStmt :: ParseStmt -> P Stmt
makeStmt (PBlock ss)            = Block <$> mapM makeStmt ss
makeStmt (PBlockingAsgn l r)    = BlockingAsgn
                                  <$> checkLhsIsVar l
                                  <*> makeExpr r
makeStmt (PNonBlockingAsgn l r) = NonBlockingAsgn 
                                  <$> checkLhsIsVar l
                                  <*> makeExpr r
makeStmt (PIfStmt cond th el)   = IfStmt
                                  <$> makeExpr cond
                                  <*> makeStmt th
                                  <*> makeStmt el
makeStmt PSkip                  = return Skip

gate2IR :: Loc -> ParseGate -> P IR
gate2IR loc (PContAsgn l r) = do
  l' <- checkLhsIsVar l
  r' <- makeExpr r
  return $ Always Assign (BlockingAsgn l' r') loc
gate2IR _ (PModuleInst{..}) = do
  irs' <- makeIntermediaryIR loc' pmInstBehaviors pmInstGates
  st' <- uses st (set irs irs')
  return $
    ModuleInst{ modInstName = pmInstName
              , modParams   = toPort <$> pmInstPorts
              , modInstSt   = st'
              }
  where
    toPort (PInput x)  = Input x
    toPort (POutput x) = Output x
    loc' = (pmModuleName, pmInstName)

makeEvent :: ParseEvent -> Event
makeEvent PStar          = Star
makeEvent (PPosEdge clk) = PosEdge clk
makeEvent (PNegEdge clk) = NegEdge clk

makeExpr :: Id -> P VExpr
makeExpr v = do
  res <- uses parseUFs (M.lookup v)
  case res of
    Nothing       -> return (VVar v)
    Just (fn, as) -> do
      as' <- SQ.fromList <$> mapM makeExpr as
      return $ VUF { vVarName  = v
                   , vFuncName = fn
                   , vFuncArgs = as'
                   }

checkLhsIsVar :: Id -> P Id
checkLhsIsVar l = do
  res <- uses parseUFs (M.lookup l)
  case res of
    Nothing -> return l
    _       -> error $ printf "lhs '%s' is a function!" l

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
symbol :: String -> Parser Id
symbol s = T.pack <$> L.symbol spaceConsumer s

comma :: Parser Id
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
rWord   :: String -> Parser Id
rWord w = (T.pack <$> string w) <* notFollowedBy alphaNumChar <* spaceConsumer

-- | list of reserved words
keywords :: [Id]
keywords =
  [ "register", "wire", "always", "link", "asn", "taint_source", "taint_sink"
  , "block", "b_asn", "nb_asn", "ite", "skip", "module", "topmodule"
  , "sanitize", "sanitize_mod"
  ]

-- | `identifier` parses identifiers: lower-case alphabets followed by alphas or digits
identifier :: Parser Id
identifier = lexeme (p >>= check)
  where
    p :: Parser Id
    p = idCons <$> letterChar <*> (T.pack <$> many nonFirstChar)

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

renderError :: IRParseError -> IO String
renderError = return . eMsg

data IRParseError = IRParseError { eMsg :: !String
                                 }
                  deriving (Show, Typeable)

instance Exception IRParseError

instance Hashable ParseVar where
  hashWithSalt n (PRegister s) = hashWithSalt n ("parse-register" :: Id, s)
  hashWithSalt n (PWire s)     = hashWithSalt n ("parse-wire" :: Id, s)


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
