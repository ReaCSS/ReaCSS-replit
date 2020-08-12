{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Data.ReaCSS where

import Data.List
import Prettyprinter

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

-- Pretty printers
instance Pretty ReaCSS where
  pretty (ReaCSS rules)
    = concatWith (\x y -> x <> hardline <> hardline <> y) (pretty <$> rules)

instance Pretty ReaCSSRule where
  pretty ReaCSSRule{..}
    = sep . mconcat $
      [ [pretty reaCSSRuleFromSelector]
      , pretty <$> reaCSSRuleBlocks
      , [pretty reaCSSRuleToSelector]
      ]

instance Pretty ReaCSSSelector where
  pretty (ReaCSSIdSelector idSelector) = "#" <> pretty idSelector
  pretty (ReaCSSClassSelector classSelector) = "." <> pretty classSelector

instance Pretty ReaCSSBlock where
  pretty (ReaCSSBlock decls)
    = sep
      [ nest 2 . sep $ "{" : ((<> ";") . pretty <$> decls)
      , "}"
      ]

instance Pretty ReaCSSDeclaration where
  pretty SyntaxForThisIsNotClearYet = "decl"