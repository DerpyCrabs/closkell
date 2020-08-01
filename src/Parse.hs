module Parse (readExpr, readExprList) where

import Control.Monad.Except
import Text.Megaparsec hiding (spaces)
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_."

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

parseString :: Parser LispVal
parseString = char '"' >> String <$> manyTill L.charLiteral (char '"')

parseAtom :: Parser LispVal
parseAtom = do
  pos <- getSourcePos
  _ <- notFollowedBy (char '.')
  first <- letterChar <|> symbol
  _ <- notFollowedBy digitChar
  rest <- many (alphaNumChar <|> symbol)
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
parseInteger = Integer <$> L.signed (return ()) (lexeme (try parseBinary <|> try parseOctal <|> try parseHexadecimal <|> parseDecimal))

parseFloat :: Parser LispVal
parseFloat = Float <$> L.signed (return ()) (lexeme L.float)

parseList :: Parser LispVal
parseList = getSourcePos >>= \pos -> List (Just pos) <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  pos <- getSourcePos
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList (Just pos) head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  pos <- getSourcePos
  char '\''
  x <- parseExpr
  return $ List (Just pos) [Atom (Just pos) "quote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  pos <- getSourcePos
  char '~'
  x <- parseExpr
  return $ List (Just pos) [Atom (Just pos) "unquote", x]

parseEmptyList :: Parser LispVal
parseEmptyList = do
  pos <- getSourcePos
  char '('
  spaces
  char ')'
  return $ List (Just pos) [Atom (Just pos) "quote", List (Just pos) []]

parseExpr :: Parser LispVal
parseExpr =
  try parseAtom
    <|> parseString
    <|> parseCharacter
    <|> try parseFloat
    <|> parseInteger
    <|> parseQuoted
    <|> parseUnquoted
    <|> try parseEmptyList
    <|> do
      spaces
      char '('
      spaces
      x <- try parseList <|> parseDottedList
      spaces
      char ')'
      spaces
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
