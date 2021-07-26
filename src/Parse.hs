module Parse (readExpr, readExprList, load) where

import Control.Monad.Except
import Data.Error
import Data.Maybe (catMaybes)
import Data.Value (atom, list)
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

lcbracket = lexeme $ char '{'

rcbracket = lexeme $ char '}'

dottedListSeparator = lexeme $ char '.'

parseString :: Parser Value
parseString = char '"' >> String <$> manyTill L.charLiteral (char '"')

parseUnit :: Parser Value
parseUnit = L.symbol spaces "unit" >> return Unit

parseAtom :: Parser Value
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

parseCharacter :: Parser Value
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

parseInteger :: Parser Value
parseInteger = Integer <$> L.signed (return ()) (try parseBinary <|> try parseOctal <|> try parseHexadecimal <|> parseDecimal)

parseFloat :: Parser Value
parseFloat = Float <$> L.signed (return ()) L.float

parseExprOrSkip = try skipExpr <|> (Just <$> parseExpr)
  where
    parseExprOrSkip = try skipExpr <|> (Just <$> parseExpr)
    skipExpr = do
      char '#' >> char '_'
      _ <- parseExpr
      return Nothing

parseCall :: Parser Value
parseCall = do
  lparen
  v <- parseCallInner
  rparen
  return v

parseCallInner :: Parser Value
parseCallInner = Call . catMaybes <$> sepBy parseExprOrSkip spaces

parseListInner :: Parser Value
parseListInner = getSourcePos >>= \pos -> List (Just pos) . catMaybes <$> sepBy parseExprOrSkip spaces

parseDottedListInner :: Parser Value
parseDottedListInner = do
  pos <- getSourcePos
  head <- manyTill parseExpr dottedListSeparator
  DottedList (Just pos) head <$> parseExpr

parseList :: Parser Value
parseList = do
  lbracket
  x <- lexeme (try parseDottedListInner <|> parseListInner)
  rbracket
  return x

parseMapInner :: Parser Value
parseMapInner = Map . catMaybes <$> sepBy parseExprOrSkip spaces

parseMap :: Parser Value
parseMap = do
  lcbracket
  map <- lexeme parseMapInner
  rcbracket
  return map

parseQuoted :: Parser Value
parseQuoted = do
  pos <- getSourcePos
  char '\''
  x <- parseExpr
  return $ Call [Atom (Just pos) "quote", x]

parseUnquoted :: Parser Value
parseUnquoted = do
  pos <- getSourcePos
  char '~'
  x <- parseExpr
  return $ Call [Atom (Just pos) "unquote", x]

parseExpr :: Parser Value
parseExpr =
  lexeme $
    parseString
      <|> parseCharacter
      <|> try parseFloat
      <|> try parseInteger
      <|> parseUnit
      <|> parseAtom
      <|> parseQuoted
      <|> parseUnquoted
      <|> parseCall
      <|> parseMap
      <|> parseList

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

load :: String -> IOThrowsError [Value]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList filename
