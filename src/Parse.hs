module Parse (readExpr, readExprList, load) where

import Control.Monad.Except
import Data.Error
import Data.Maybe (catMaybes)
import Text.Megaparsec hiding (spaces)
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?^_"

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

lbracket = lexeme $ char '['

rbracket = lexeme $ char ']'

lparen = lexeme $ char '('

rparen = lexeme $ char ')'

dottedListSeparator = lexeme $ char '.'

parseString :: Parser LispVal
parseString = char '"' >> String <$> manyTill L.charLiteral (char '"')

parseUnit :: Parser LispVal
parseUnit = L.symbol spaces "unit" >> return Unit

parseAtom :: Parser LispVal
parseAtom = do
  pos <- getSourcePos
  first <- letterChar <|> symbol
  rest <- many (alphaNumChar <|> symbol <|> char '.')
  let atom = first : rest
  return $ case atom of
    "true" -> Bool True
    "false" -> Bool False
    _ -> Atom (Just pos) atom

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

parseUnicodeCharacter :: Parser Char
parseUnicodeCharacter = toEnum . read <$> count 4 digitChar

parseCharacter :: Parser LispVal
parseCharacter =
  Character
    <$> ( char '\\'
            >> try (L.symbol spaces "newline" >> return '\n')
            <|> try (L.symbol spaces "space" >> return ' ')
            <|> try (L.symbol spaces "tab" >> return '\t')
            <|> try (L.symbol spaces "formfeed" >> return '\f')
            <|> try (L.symbol spaces "backspace" >> return '\b')
            <|> try (L.symbol spaces "return" >> return '\r')
            <|> try parseUnicodeCharacter
            <|> alphaNumChar
        )

parseBinary :: Parser Integer
parseBinary = char '0' >> char 'b' >> L.binary

parseOctal :: Parser Integer
parseOctal = char '0' >> char 'o' >> L.octal

parseHexadecimal :: Parser Integer
parseHexadecimal = char '0' >> char 'x' >> L.hexadecimal

parseDecimal :: Parser Integer
parseDecimal = L.decimal

parseInteger :: Parser LispVal
parseInteger = Integer <$> L.signed (return ()) (try parseBinary <|> try parseOctal <|> try parseHexadecimal <|> parseDecimal)

parseFloat :: Parser LispVal
parseFloat = Float <$> L.signed (return ()) L.float

parseExprOrSkip = try skipExpr <|> (Just <$> parseExpr)
  where
    parseExprOrSkip = try skipExpr <|> (Just <$> parseExpr)
    skipExpr = do
      char '#' >> char '_'
      _ <- parseExpr
      return Nothing

parseList :: Parser LispVal
parseList = getSourcePos >>= \pos -> List (Just pos) . catMaybes <$> sepBy parseExprOrSkip spaces

parseCall :: Parser LispVal
parseCall = do
  lparen
  v <- parseCallInner
  rparen
  return v

parseCallInner :: Parser LispVal
parseCallInner = Call . catMaybes <$> sepBy parseExprOrSkip spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  pos <- getSourcePos
  head <- manyTill parseExpr dottedListSeparator
  DottedList (Just pos) head <$> parseExpr

parseQuoted :: Parser LispVal
parseQuoted = do
  pos <- getSourcePos
  char '\''
  x <- parseExpr
  return $ Call [Atom (Just pos) "quote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  pos <- getSourcePos
  char '~'
  x <- parseExpr
  return $ Call [Atom (Just pos) "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  pos <- getSourcePos
  char '~' >> char '@'
  x <- parseExpr
  return $ Call [Atom (Just pos) "unquote-splicing", x]

parseLambdaShorthand :: Parser LispVal
parseLambdaShorthand = do
  pos <- getSourcePos
  char '#' >> lparen
  inner <- lexeme parseCallInner
  rparen
  return $ Call (Atom Nothing "fn" : DottedList Nothing [] (Atom Nothing "%&") : [inner])

parseLambdaShorthandArgs =
  try parseSingle <|> parseNth
  where
    parseSingle = do
      char '%' >> char '%'
      return (Call [Atom Nothing "car", Atom Nothing "%&"])
    parseNth :: Parser LispVal
    parseNth = do
      char '%'
      n <- digitChar
      return (Call [Atom Nothing "nth", Integer (read [n] - 1), Atom Nothing "%&"])

parseExpr :: Parser LispVal
parseExpr =
  lexeme $
    try parseLambdaShorthandArgs
      <|> parseUnit
      <|> parseAtom
      <|> parseLambdaShorthand
      <|> parseString
      <|> parseCharacter
      <|> try parseFloat
      <|> parseInteger
      <|> parseQuoted
      <|> try parseUnquoteSplicing
      <|> parseUnquoted
      <|> parseCall
      <|> do
        lbracket
        x <- lexeme (try parseDottedList <|> parseList)
        rbracket
        return x

readOrThrow :: Parser a -> String -> String -> ThrowsError a
readOrThrow parser file input = case parse parser file input of
  Left err -> throwError $ Parsing err
  Right val -> return val

readExpr = readOrThrow parseExpr

readExprList = readOrThrow $ do
  _ <- spaces
  exprs <- sepEndBy parseExpr spaces
  _ <- eof
  return exprs

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList filename
