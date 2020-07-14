module Parse (readExpr, readExprList) where

import Control.Monad.Except
import Text.Megaparsec hiding (spaces)
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  pos <- getSourcePos
  first <- letterChar <|> symbol
  rest <- many (alphaNumChar <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom (Just pos) atom

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

parseNumber :: Parser LispVal
parseNumber = Number <$> lexeme L.decimal

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

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
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
