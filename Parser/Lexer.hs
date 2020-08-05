module Parser.Lexer where

import Control.Monad
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

import qualified Text.Parsec.Char as TPC

identifier :: Parser String
identifier = (:) <$> TPC.letter <*> many TPC.alphaNum

encodedString :: Parser String
encodedString = many $ noneOf [openAngularBraceToken, closeAngularBraceToken]

quotedString :: Parser String
quotedString = qString '"' <|> qString '\''
  where
   qString :: Char -> Parser String
   qString q = TPC.char q *> many (TPC.satisfy (/= q)) <* TPC.char q

symbol :: String -> Parser ()
symbol = lexeme . void . TPC.string

lexeme :: Parser a -> Parser a
lexeme x = x <* TPC.spaces

openCurlyBraceToken :: Char
openCurlyBraceToken = '{'

closeCurlyBraceToken :: Char
closeCurlyBraceToken = '}'

openAngularBraceToken :: Char
openAngularBraceToken = '<'

openAngularBraceSlashTokens :: String
openAngularBraceSlashTokens = openAngularBraceToken : '/' : []

closeAngularBraceToken :: Char
closeAngularBraceToken = '>'

equalToken :: Char
equalToken = '='