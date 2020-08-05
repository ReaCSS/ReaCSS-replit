module Data.ReaXML where

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
  = ReaXMLElement ReaXMLName ReaXMLAttributes ReaXMLTree
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