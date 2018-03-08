module SMT2FP.SMT.Parser ( parse
                         , CmdParseError(..)
                         , renderError
                         ) where

import           Control.Exception
import           Control.Monad (void)
import           Data.Char (isLetter, isDigit)
import qualified Data.List.NonEmpty         as NE
import           Data.Typeable
import           Text.Megaparsec            as MP hiding (parse, State(..))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

import           SMT2FP.SMT.Types

type Parser = Parsec SourcePos String

-- --------------------------------------------------------------------------------
parse :: FilePath -> String -> [Command]
-- --------------------------------------------------------------------------------
parse f = parseWith (some parseCommand) f

parseWith  :: Parser a -> FilePath -> String -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> throw (CmdParseError (parseErrorPretty err) (NE.head . errorPos $ err))
                    Right e  -> e

parseCommand :: Parser Command
parseCommand = parens $
               (rWord "set-logic" *> (SetLogic <$> identifier)) <|>
               (rWord "declare-fun" *> (DeclareFun <$>
                                         identifier <*>
                                         parens (many parseType) <*>
                                         parseType
                                       )) <|>
               (rWord "declare-relation" *> (DeclareFun <$>
                                             identifier <*>
                                             parens (many parseType) <*>
                                             (return B)
                                            )) <|>
               (rWord "declare-const" *> (DeclareConst <$>
                                          identifier <*>
                                          parseType
                                       )) <|>
               (rWord "assert" *> (Assert <$> parseTerm)) <|>
               (rWord "check-sat" *> return CheckSat) <|>
               (rWord "get-model" *> return GetModel)
               

parseType :: Parser Type
parseType = ( rWord "Bool" *> return B ) <|>
            ( rWord "Int" *> return I ) <|>
            parens ( rWord "Array" *> (arrayFromList <$> (atLeastN 2 parseType)))
  where
    arrayFromList ts = A (init ts) (last ts)
    
parseSortedVar :: Parser SortedVar
parseSortedVar = parens $ SortedVar <$> identifier <*> parseType

parseBinOp :: Parser BinOp
parseBinOp = (symbol "+"      *> return PLUS)    <|>
             (symbol "=>"     *> return IMPLIES) <|>
             (symbol "="      *> return EQU)     <|>
             (symbol ">="     *> return GE)

parseTerm :: Parser Term
parseTerm = parseConst <|>
            parens (haveParen <|> parseTerm) 
            
  where
    parseApp f [] = Var f
    parseApp f as = App f as

    haveParen = rWord "forall" *> (Forall <$>
                                   parens (many parseSortedVar) <*>
                                   parseTerm) <|>
                BinOp <$> parseBinOp <*> parseTerm <*> parseTerm <|>
                rWord "ite" *> (Ite <$> parseTerm <*> parseTerm <*> parseTerm) <|>
                rWord "and" *> (Ands <$> some parseTerm) <|>
                rWord "select" *> (Select <$> identifier <*> some identifier) <|>
                parseApp <$> identifier <*> many identifier

    parseConst = Var <$> identifier <|>
                 Number <$> L.decimal <|>
                 rWord "true"  *> return (Boolean True) <|>
                 rWord "false" *> return (Boolean False)
              

--------------------------------------------------------------------------------
-- | Tokenisers and Whitespace
--------------------------------------------------------------------------------

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = spaceConsumer *> p <* eof

spaceConsumer :: Parser ()
spaceConsumer = (L.space (void spaceChar) lineCmnt empty)
  where -- blockCmnt    = L.skipBlockComment "/*" "*/"
        lineCmnt     = L.skipLineComment ";"
        -- prologDecl   = L.skipLineComment ":-"

-- | `symbol s` parses just the string s (and trailing whitespace)
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = betweenS "(" ")"

-- comma :: Parser String
-- comma = symbol ","

-- list :: Parser a -> Parser [a]
-- list p = betweenS "[" "]" lp
--   where
--     lp = (:) <$> p <*> many (comma *> p)
--          <|> return []

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
  [ "set-logic", "declare-fun", "declare-relation", "assert", "forall", "check-sat", "get-model"
  , "and", "select"
  , "true", "false"
  , "Int", "Bool", "Array"
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
safeHead :: a -> [a] -> a
safeHead def []    = def
safeHead _   (x:_) = x

getRange :: Int -> Int -> [a] -> [a]
getRange i1 i2
  = take (i2 - i1 + 1)
  . drop (i1 - 1)

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
    nD       = length nS

renderError :: CmdParseError -> IO String
renderError e = do
  let pos = ePos e
      msg = eMsg e
  snippet <- readFilePos pos
  return $ printf "%s%s" snippet msg

instance ShowErrorComponent SourcePos where
  showErrorComponent pos = "parse error in file " ++ (MP.sourceName pos)

data CmdParseError = CmdParseError
  { eMsg :: !String
  , ePos :: !MP.SourcePos
  }
  deriving (Show, Typeable)

instance Exception CmdParseError

atLeastN :: Int -> Parser a -> Parser [a]
atLeastN 0 a = many a
atLeastN n a = (:) <$> a <*> atLeastN (n-1) a
