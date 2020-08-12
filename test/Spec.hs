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
    [ golden "reaxml0.txt" "reaxml0.reprinted.txt" (testParserWithFile reaXML)
    , golden "reacss0.txt" "reacss0.reprinted.txt" (testParserWithFile reaCSS)
    ]
  where
    testParserWithFile :: (Pretty a) => Parser a -> FilePath -> FilePath -> IO ByteString
    testParserWithFile p fp _ = do
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

golden :: FilePath -> FilePath -> (FilePath -> FilePath -> IO ByteString) -> TestTree
golden inputFn outputFn writing
  = goldenVsStringDiff
    (testFixturePath inputFn)
    goldenGitDiff
    (testFixturePath outputFn)
    (writing (testFixturePath inputFn) (testFixturePath outputFn))
  where
    goldenGitDiff :: FilePath -> FilePath -> [String]
    goldenGitDiff ref new
      = ["git", "diff", "--no-index", "--text", "--exit-code", ref, new]

testFixturePath :: FilePath -> FilePath
testFixturePath = ("test/fixture/" <>)
