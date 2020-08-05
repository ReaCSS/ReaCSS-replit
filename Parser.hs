module Parser where

import Parser.ReaCSS
import Parser.ReaXML
import Text.Parsec

testReaXML :: FilePath -> IO ()
testReaXML fp = do
  str <- readFile fp
  parseTest reaXML str