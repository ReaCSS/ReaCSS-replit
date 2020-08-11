{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Compiler.ReaXMLToECMA where

import Data.List
import Data.ReaXML
import Prettyprinter

type Compiler a = a -> Doc ()

--
-- A Naive Compiler for ReaXML
--

compileReaXML :: Compiler ReaXML
compileReaXML (ReaXML t)
  = "(function(){"
  <> "var n=document;"
  <> compileReaXMLTree t
  <> "})();"

-- Use methods that are common for Element interface AND Document interface only.
compileReaXMLTree :: Compiler ReaXMLTree
compileReaXMLTree t
  = "(function(p){"
    <> "var cs=[" <> concatWith (surround ",") (compileReaXMLNode <$> t) <> "];"
    <> "var i=0;"
    <> "for(i=0;i<" <> pretty (length t) <> ";i++){"
    <> "p.appendChild(cs[i]);"
    <> "}"
    <> "})(n);"

compileReaXMLNode :: Compiler ReaXMLNode
compileReaXMLNode (ReaXMLElementNode el) = compileReaXMLElement el
compileReaXMLNode (ReaXMLTextNode text) = compileReaXMLText text

compileReaXMLElement :: Compiler ReaXMLElement
compileReaXMLElement ReaXMLElement{..}
  | "q" `isPrefixOf` reaXMLElementName
    = "(function(){"
    <> "var n=document.createElement(" <> dquotes (pretty reaHTMLElementName) <> ");"
    <> hcat ((<> ";") . compileReaXMLAttribute <$> reaXMLElementAttributes)
    <> compileReaXMLTree reaXMLElementChildren
    <> "})();"
  | otherwise
    = "(function(){throw new Error(\"Not yet implemented\");})();"
  where
    reaHTMLElementName = drop 1 reaXMLElementName

compileReaXMLAttribute :: Compiler ReaXMLAttribute
compileReaXMLAttribute ReaXMLAttribute{..}
  = "n.setAttribute(" <> pretty reaXMLAttributeName <> "," <> pretty reaXMLAttributeValue <> ")"

compileReaXMLText :: Compiler ReaXMLText
compileReaXMLText text = "document.createTextNode(" <> dquotes (pretty text) <> ")"