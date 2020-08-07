module Parser where

import Parser.ReaCSS
import Parser.ReaXML
import Text.Megaparsec
import Prettyprinter (Pretty, pretty)
import Prettyprinter.Util (putDocW)

testReaCSS :: FilePath -> IO ()
testReaCSS = testParserWithFile reaCSS

testReaXML :: FilePath -> IO ()
testReaXML = testParserWithFile reaXML

testParserWithFile :: (Pretty a) => Parser a -> FilePath -> IO ()
testParserWithFile p fp = do
  str <- readFile fp
  case parse p fp str of
    Left err -> print err
    Right v -> putDocW 80 $ pretty v