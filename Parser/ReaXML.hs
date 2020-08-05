module Parser.ReaXML where

import Data.ReaXML

import Text.Parsec

type Parser = Parsec String ()

reaXML :: Parser ReaXML
reaXML = undefined

reaXMLTree :: Parser ReaXMLTree
reaXMLTree = undefined

reaXMLNode :: Parser ReaXMLNode
reaXMLNode = undefined