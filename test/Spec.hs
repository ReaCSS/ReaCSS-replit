module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Parser
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
    testParserWithFile :: (Pretty a) => Parser a -> FilePath -> FilePath -> IO Text
    testParserWithFile p fp _ = do
      str <- readFile fp
      pure $ parseAndSerialize p fp str

    parseAndSerialize :: (Pretty a) => Parser a -> String -> String -> Text
    parseAndSerialize p name str
      = case parse p name str of
          Left err -> T.pack $ show err
          Right v -> renderStrict . layoutPretty defaultLayoutOptions $ pretty v

golden :: FilePath -> FilePath -> (FilePath -> FilePath -> IO Text) -> TestTree
golden inputFn outputFn writing
  = goldenVsStringDiff
    (testFixturePath inputFn)
    goldenGitDiff
    (testFixturePath outputFn)
    ( BSL.fromStrict
      . TE.encodeUtf8
      <$> writing (testFixturePath inputFn) (testFixturePath outputFn)
    )
  where
    goldenGitDiff :: FilePath -> FilePath -> [String]
    goldenGitDiff ref new
      = ["git", "diff", "--no-index", "--text", "--exit-code", ref, new]

testFixturePath :: FilePath -> FilePath
testFixturePath = ("test/fixture/" <>)
