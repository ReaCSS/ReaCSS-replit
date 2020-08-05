module Parser.Lexer where

import Control.Monad
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

import qualified Text.Parsec.Char as TPC

identifier :: Parser String
identifier = (:) <$> TPC.letter <*> many TPC.alphaNum

encodedString1 :: Parser String
encodedString1 = many1 $ noneOf [openAngularBraceToken, closeAngularBraceToken]

quotedString :: Parser String
quotedString = qString doubleQuoteToken <|> qString singleQuoteToken
  where
   qString :: Char -> Parser String
   qString q = TPC.char q *> many (TPC.satisfy (/= q)) <* TPC.char q

symbol :: String -> Parser ()
symbol = lexeme . void . TPC.string

lexeme :: Parser a -> Parser a
lexeme x = x <* TPC.spaces

-- Char Token(s)

openAngularBraceSlashTokens :: String
openAngularBraceSlashTokens = openAngularBraceToken : slashToken : []

slashCloseAngularBraceToken :: String
slashCloseAngularBraceToken = slashToken : closeAngularBraceToken : []

singleQuoteToken :: Char
singleQuoteToken = '\''

doubleQuoteToken :: Char
doubleQuoteToken = '"'

openCurlyBraceToken :: Char
openCurlyBraceToken = '{'

closeCurlyBraceToken :: Char
closeCurlyBraceToken = '}'

openAngularBraceToken :: Char
openAngularBraceToken = '<'

closeAngularBraceToken :: Char
closeAngularBraceToken = '>'

slashToken :: Char
slashToken = '/'

equalToken :: Char
equalToken = '='