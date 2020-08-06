module Text.PrettyPrint.ReaCSSExtension
  ( module Text.PrettyPrint.HughesPJClass
  , vsep
  )
where

import Text.PrettyPrint.HughesPJClass hiding ((<>))

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty