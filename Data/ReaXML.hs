module Data.ReaXML where

newtype ReaXML
  = ReaXML ReaXMLTree
  deriving ( Show
           )

type ReaXMLTree
  = [ReaXMLNode]

data ReaXMLNode
  = ReaXMLNode ReaXMLNodeName ReaXMLAttributes ReaXMLTree
  deriving ( Show
           )

type ReaXMLNodeName = String

type ReaXMLAttributes = [ReaXMLAttribute]

type ReaXMLAttribute = String