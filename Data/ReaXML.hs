{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Data.ReaXML where

import Text.PrettyPrint.HughesPJClass hiding ((<>))

newtype ReaXML
  = ReaXML ReaXMLTree
  deriving ( Show
           )

type ReaXMLTree
  = [ReaXMLNode]

data ReaXMLNode
  = ReaXMLElementNode ReaXMLElement
  | ReaXMLTextNode ReaXMLText
  deriving ( Show
           )

data ReaXMLElement
  = ReaXMLElement
    { reaXMLElementName :: ReaXMLName
    , reaXMLElementAttributes :: ReaXMLAttributes
    , reaXMLElementChildren :: ReaXMLTree
    }
  deriving ( Show
           )

type ReaXMLText = String

type ReaXMLAttributes = [ReaXMLAttribute]

data ReaXMLAttribute
  = ReaXMLAttribute
    { reaXMLAttributeName :: ReaXMLName
    , reaXMLAttributeValue :: ReaXMLValue
    }
  deriving ( Show
           )

type ReaXMLName = String
type ReaXMLValue = String

-- Pretty printers
instance Pretty ReaXML where
  pPrint (ReaXML reaXMLTree) = vsep (pPrint <$> reaXMLTree)

instance Pretty ReaXMLNode where
  pPrint (ReaXMLTextNode reaXMLText) = text reaXMLText
  pPrint (ReaXMLElementNode reaXMLElement) = pPrint reaXMLElement

instance Pretty ReaXMLElement where
  pPrint ReaXMLElement{..}
    = if null reaXMLElementChildren
      then
        sep
        [ "<" <> text reaXMLElementName
        , nest 2 attributes
        , "/>"
        ]
      else
        cat
        [
          sep
          [ "<" <> text reaXMLElementName
          , nest 2 attributes
          ]
        , ">"
        ]
        $+$ nest 2 children
        $+$ "</" <> text reaXMLElementName <> ">"
    where
      attributes = sep (pPrint <$> reaXMLElementAttributes)
      children = vsep (pPrint <$> reaXMLElementChildren)

instance Pretty ReaXMLAttribute where
  pPrint ReaXMLAttribute{..}
    = text reaXMLAttributeName <> equals <> doubleQuotes (text reaXMLAttributeValue)

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty