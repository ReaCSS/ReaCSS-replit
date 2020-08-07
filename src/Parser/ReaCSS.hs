module Parser.ReaCSS
  ( module Parser.ReaCSS
  , module Parser.Lexer
  ) where

import Data.ReaCSS
import Parser.Lexer
import Text.Megaparsec

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
  c <- anySingle
  case c of
    '#' -> ReaCSSIdSelector <$> reaCSSName
    '.' -> ReaCSSClassSelector <$> reaCSSName
    _ -> fail $ "The selector should start with '#' or '.', but it starts with '" <> pure c <> "'."

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