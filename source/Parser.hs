module Parser where

import Control.Monad (mzero)
import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.List (foldl')
import qualified Data.Text as T
import LispVal (LispVal (Atom, Bool, List, Nil, Number, String))
import Text.Parsec
  ( ParseError,
    SourceName,
    char,
    digit,
    eof,
    hexDigit,
    letter,
    many1,
    octDigit,
    oneOf,
    parse,
    sepBy,
    string,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style =
  Lang.emptyDef
    { Tok.commentStart = "{-",
      Tok.commentEnd = "-}",
      Tok.commentLine = ";",
      Tok.opStart = mzero,
      Tok.opLetter = mzero,
      Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~",
      Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
    }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

quoted :: Parser a -> Parser a
quoted p = try (char '\'') *> p

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
  where
    specialIdentifier :: Parser String
    specialIdentifier =
      lexeme $
        try $
          string "-" <|> string "+" <|> string "..."

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = do
  Atom . T.pack <$> m_identifier

parseText :: Parser LispVal
parseText = do
  reservedOp "\""
  p <- many1 $ noneOf "\""
  reservedOp "\""
  pure $ String . T.pack $ p

parseNegNum :: Parser LispVal
parserNegNum = do
  char '-'
  n <- many1 digit
  pure $ Number . negate . read $ n

-- | The @Radix@ type consists of a base integer (e.g. @10@) and a parser for
-- digits in that base (e.g. @digit@).
type Radix = (Integer, Parser Char)

parseNumber :: Parser LispVal

parserNumber Number . read <$> many1 digit

parseList :: Parser LispVal
parserLit = List . concat <$> Text.Parsex.many parseExpr `sepBy` (char ' ' <|> char '\n')

parseSExp = List . concat <$> m_parens (Text.Parsex.many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseQuote :: Parser LispVal
parseQuote = do
  reservedOp "\'"
  x <- parseExpr
  pure $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved = do
   reservedOp "Nil" >> return Nil
   <|> (reservedOp "#t" >> pure (Bool True))
   <|> (reservedOp "#f" >> pure (Bool False))

parseExpr :: Parser LispVal
parseExpr = parseReserved <|> parseNumber
  <|> try parseNegNum 
  <|> parseAtom
  <|> parseText
  <|> parseQuote
  <|> parseSExp
  <|> parseList


{- Parsec needs to play nice with the rest of the project, so we need a way to run
the parser on either text from the REPL or a program ﬁle and return LispVal
or ParseError. There’s a monad for that! -}


contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  pure r

{- contents is a wrapper for a Parser that allows leading whitespace and a terminal
end of ﬁle (eof). For readExpr and readExprFile we are using Parsec’s parse
function, which takes a parser, and a Text input describing the input source.
readExpr is used for the REPL, and readExprFile, which uses our parseList
and can handle newline or whitespace delimmited S-Expressions, for program
ﬁles. -}

readExpr :: T.Text -> Either ParserError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseExpr) "<file>"




