{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Data.ReaCSS where

import Data.List
import Text.PrettyPrint.HughesPJClass hiding ((<>))

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
  pPrint (ReaCSS rules)
    = vsep
      (intersperse "" (pPrint <$> rules))

instance Pretty ReaCSSRule where
  pPrint ReaCSSRule{..}
    = pPrint reaCSSRuleFromSelector
    $+$ vsep (pPrint <$> reaCSSRuleBlocks)
    $+$ pPrint reaCSSRuleToSelector

instance Pretty ReaCSSSelector where
  pPrint (ReaCSSIdSelector idSelector) = "#" <> text idSelector
  pPrint (ReaCSSClassSelector classSelector) = "." <> text classSelector

instance Pretty ReaCSSBlock where
  pPrint (ReaCSSBlock decls) =
    sep
    [ "{"
    , nest 2 (sep (punctuate ";" (pPrint <$> decls)))
    , "}"
    ]

instance Pretty ReaCSSDeclaration where
  pPrint SyntaxForThisIsNotClearYet = "decl"

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty