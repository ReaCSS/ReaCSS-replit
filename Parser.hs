module Parser where

import Parser.ReaCSS
import Parser.ReaXML
import Text.Parsec
import Text.PrettyPrint.HughesPJClass (prettyShow)

testReaXML :: FilePath -> IO ()
testReaXML fp = do
  str <- readFile fp
  case parse reaXML fp str of
    Left err -> print err
    Right reaXML -> putStrLn $ prettyShow reaXML