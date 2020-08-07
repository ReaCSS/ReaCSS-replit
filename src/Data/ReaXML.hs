{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Data.ReaXML where

import Prettyprinter

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
  pretty (ReaXML reaXMLTree) = vsep (pretty <$> reaXMLTree)

instance Pretty ReaXMLNode where
  pretty (ReaXMLTextNode reaXMLText) = pretty reaXMLText
  pretty (ReaXMLElementNode reaXMLElement) = pretty reaXMLElement

instance Pretty ReaXMLElement where
  pretty ReaXMLElement{..}
    = if null reaXMLElementChildren
      then
        sep
        [ "<" <> pretty reaXMLElementName
        , indent 2 attributes
        , "/>"
        ]
      else
        vsep
        [ cat
          [
            sep
            [ "<" <> pretty reaXMLElementName
            , indent 2 attributes
            ]
          , ">"
          ]
        , indent 2 children
        , "</" <> pretty reaXMLElementName <> ">"
        ]
    where
      attributes = sep (pretty <$> reaXMLElementAttributes)
      children = vsep (pretty <$> reaXMLElementChildren)

instance Pretty ReaXMLAttribute where
  pretty ReaXMLAttribute{..}
    = pretty reaXMLAttributeName <> equals <> dquotes (pretty reaXMLAttributeValue)