module Main where

import Data.ByteString.Lazy (ByteString)
import Parser.ReaCSS
import Parser.ReaXML
import Test.Tasty
import Test.Tasty.Golden
import Text.Megaparsec
import Prettyprinter (Pretty(pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ReaCSS" [parserTests]

parserTests :: TestTree
parserTests
  = testGroup "Parser"
    [ golden "test/fixture/reaxml0.txt" (testParserWithFile reaXML)
    , golden "test/fixture/reacss0.txt" (testParserWithFile reaCSS)
    ]
  where
    testParserWithFile :: (Pretty a) => Parser a -> FilePath -> IO ByteString
    testParserWithFile p fp = do
      str <- readFile fp
      case parse p fp str of
        Left err ->
          pure
          . BSL.fromStrict
          . TE.encodeUtf8
          . T.pack
          $ show err
        Right v ->
          pure
          . BSL.fromStrict
          . TE.encodeUtf8
          . renderStrict
          . layoutPretty defaultLayoutOptions
          $ pretty v

golden :: FilePath -> (FilePath -> IO ByteString) -> TestTree
golden fp writing = goldenVsStringDiff fp goldenGitDiff (goldenPathOf fp) (writing fp)
  where
    goldenPathOf :: FilePath -> FilePath
    goldenPathOf = (<> ".golden")

    goldenGitDiff :: FilePath -> FilePath -> [String]
    goldenGitDiff ref new = ["git", "diff", "--no-index", "--text", "--exit-code", ref, new]
