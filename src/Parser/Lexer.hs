module Parser.Lexer where

import Control.Monad
import Data.Void
import Text.Megaparsec

import qualified Text.Megaparsec.Char as TM
import qualified Text.Megaparsec.Char.Lexer as TML

type Parser = Parsec Void String

identifier :: Parser String
identifier = (:) <$> TM.letterChar <*> many TM.alphaNumChar

encodedString1 :: Parser String
encodedString1 = some $ noneOf [openAngularBraceToken, closeAngularBraceToken]

quotedString :: Parser String
quotedString = qString doubleQuoteToken <|> qString singleQuoteToken
  where
   qString :: Char -> Parser String
   qString q = TM.char q *> many (satisfy (/= q)) <* TM.char q

symbol :: String -> Parser ()
symbol = lexeme . void . TM.string

lexeme :: Parser a -> Parser a
lexeme = TML.lexeme sc

sc :: Parser ()
sc = void TM.space

-- Char Token(s)

openAngularBraceSlashTokens :: String
openAngularBraceSlashTokens = openAngularBraceToken : slashToken : []

slashCloseAngularBraceToken :: String
slashCloseAngularBraceToken = slashToken : closeAngularBraceToken : []

singleQuoteToken :: Char
singleQuoteToken = '\''

doubleQuoteToken :: Char
doubleQuoteToken = '"'

semicolonToken :: Char
semicolonToken = ';'

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