module Main where

import Parser.ReaCSS
import Parser.ReaXML
import Text.Megaparsec
import Prettyprinter (Pretty, pretty)
import Prettyprinter.Util (putDocW)

main :: IO ()
main = do
  testReaXML "test/fixture/reaxml0.txt"
  testReaCSS "test/fixture/reacss0.txt"

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