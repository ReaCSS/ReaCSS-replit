module Data.ReaCSS where

newtype ReaCSS
  = ReaCSS [ReaCSSRule]
  deriving ( Show
           )

data ReaCSSRule
  = ReaCSSRule
    { reaCSSRuleFromSelector :: ReaCSSSelector
    , reaCSSRuleBlocks :: [ReaCSSBlock]
    , reaCSSRuleToSelector :: ReaCSSSelector
    }
  deriving ( Show
           )

data ReaCSSSelector
  = ReaCSSIdSelector ReaCSSName
  | ReaCSSClassSelector ReaCSSName -- Do we really want this?
  deriving ( Show
           )

newtype ReaCSSBlock
  = ReaCSSBlock [ReaCSSDeclaration]
  deriving ( Show
           )

data ReaCSSDeclaration
  = SyntaxForThisIsNotClearYet
  deriving ( Show
           )

type ReaCSSName = String