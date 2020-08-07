module Parser.ReaXML
  ( module Parser.ReaXML
  , module Parser.Lexer
  ) where

import Data.ReaXML
import Parser.Lexer
import Text.Megaparsec

reaXML :: Parser ReaXML
reaXML = ReaXML <$> reaXMLTree <* eof

reaXMLTree :: Parser ReaXMLTree
reaXMLTree = many reaXMLNode

reaXMLNode :: Parser ReaXMLNode
reaXMLNode
  = ReaXMLTextNode <$> reaXMLText
  <|> ReaXMLElementNode <$> reaXMLElement

reaXMLText :: Parser ReaXMLText
reaXMLText = encodedString1

reaXMLElement :: Parser ReaXMLElement
reaXMLElement = do
  n <- try $ do
    symbol $ pure openAngularBraceToken
    reaXMLName
  a <- many reaXMLAttribute
  cs <- (symbol slashCloseAngularBraceToken >> pure [])
    <|> do
      symbol $ pure closeAngularBraceToken
      cs <- reaXMLTree
      symbol openAngularBraceSlashTokens
      symbol n
      symbol $ pure closeAngularBraceToken
      pure cs
  pure $ ReaXMLElement n a cs

reaXMLAttribute :: Parser ReaXMLAttribute
reaXMLAttribute = do
  n <- reaXMLName
  symbol $ pure equalToken
  v <- reaXMLValue
  pure $ ReaXMLAttribute n v

reaXMLName :: Parser ReaXMLName
reaXMLName = lexeme identifier

reaXMLValue :: Parser ReaXMLValue
reaXMLValue = lexeme quotedString