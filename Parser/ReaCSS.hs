module Parser.ReaCSS where

import Control.Monad
import Data.ReaCSS
import Parser.Lexer
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

import qualified Text.Parsec.Char as TPC

reaCSS :: Parser ReaCSS
reaCSS = ReaCSS <$> many reaCSSRule

reaCSSRule :: Parser ReaCSSRule
reaCSSRule = do
  from <- reaCSSSelector
  blks <- many reaCSSBlock
  to <- reaCSSSelector
  pure $ ReaCSSRule from blks to

reaCSSSelector :: Parser ReaCSSSelector
reaCSSSelector = do
  c <- TPC.anyChar
  case c of
    '#' -> ReaCSSIdSelector <$> reaCSSName
    '.' -> ReaCSSClassSelector <$> reaCSSName
    _ -> unexpected $ "The selector should start with '#' or '.', but it starts with '" <> pure c <> "'."

reaCSSBlock :: Parser ReaCSSBlock
reaCSSBlock = do
  symbol $ pure openCurlyBraceToken
  ds <- sepEndBy reaCSSDeclaration (symbol $ pure semicolonToken)
  symbol $ pure closeCurlyBraceToken
  pure $ ReaCSSBlock ds

reaCSSDeclaration :: Parser ReaCSSDeclaration
reaCSSDeclaration = do
  skipMany $ noneOf [openCurlyBraceToken, semicolonToken, closeCurlyBraceToken]
  pure SyntaxForThisIsNotClearYet

reaCSSName :: Parser ReaCSSName
reaCSSName = lexeme identifier