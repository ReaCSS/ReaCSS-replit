module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.Golden
import Text.Megaparsec
import Prettyprinter (Pretty(pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import qualified Compiler.ReaXMLToECMA as C
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Parser as P

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ReaCSS" [parserTests, compilerTests]

parserTests :: TestTree
parserTests
  = testGroup "Parser"
    [ golden "reaxml0.txt" "reaxml0.reprinted.txt" $
        testParserWithFile P.reaXML
    , golden "reacss0.txt" "reacss0.reprinted.txt" $
        testParserWithFile P.reaCSS
    ]
  where
    testParserWithFile :: (Pretty a) => P.Parser a -> FilePath -> FilePath -> IO Text
    testParserWithFile p fp _ = do
      str <- readFile fp
      pure $ parseAndSerialize p fp str

    parseAndSerialize :: (Pretty a) => P.Parser a -> String -> String -> Text
    parseAndSerialize p name str
      = case parse p name str of
          Left err -> T.pack $ show err
          Right v ->
            renderStrict
            . layoutPretty defaultLayoutOptions
            $ pretty v

compilerTests :: TestTree
compilerTests
  = testGroup "Compiler"
    [ golden "reaxml0.txt" "reaxml0.compiled.js" $
        testCompilerWithFile P.reaXML C.compileReaXML
    ]
  where
    testCompilerWithFile :: P.Parser a -> C.Compiler a -> FilePath -> FilePath -> IO Text
    testCompilerWithFile p c fp _ = do
      str <- readFile fp
      pure $ parseCompileAndSerialize p c fp str

    parseCompileAndSerialize :: P.Parser a -> C.Compiler a -> String -> String -> Text
    parseCompileAndSerialize p c name str
      = case parse p name str of
          Left err -> T.pack $ show err
          Right v ->
            renderStrict
            . layoutPretty defaultLayoutOptions
            $ c v

golden :: FilePath -> FilePath -> (FilePath -> FilePath -> IO Text) -> TestTree
golden inputFn outputFn writing
  = goldenVsStringDiff
    inputFn
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
