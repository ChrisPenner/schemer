module Parser where
{-# language OverloadedStrings #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Text as T
import Data.Void
import AST

type Parser = Parsec Void T.Text

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

var :: Parser String
var = many alphaNumChar

parseExpr :: Parser LispVal
parseExpr = choice [parseAtom, parseString, parseNumber, parseQuoted, parseLists]

parseString :: Parser LispVal
parseString = String <$> between (char '"') (char '"') (many (noneOf "\""))

parseAtom :: Parser LispVal
parseAtom = do
 start <- letterChar <|> symbol
 rest <- many (letterChar <|> symbol <|> digitChar)
 return $ case (start:rest) of
            "#t" -> Bool True
            "#f" -> Bool False
            atom -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number <$> decimal

parseList :: Parser LispVal
parseList = List <$> parseExpr `sepBy` space

parseDottedList :: Parser LispVal
parseDottedList = do
    start <- endBy parseExpr space
    rest <- char '.' >> space >> parseExpr
    return $ DottedList start rest

parseLists :: Parser LispVal
parseLists = do
  between (char '(') (char ')') $ do
    try parseList <|> parseDottedList

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: T.Text -> LispVal
readExpr txt =
  case parse parseExpr "" txt of
    Left err -> Error $ show err
    Right res -> res
