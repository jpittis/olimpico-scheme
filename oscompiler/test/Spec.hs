{-# LANGUAGE LambdaCase #-}
import Test.Hspec
import Test.HUnit.Base (assertFailure)
import Text.Megaparsec (parse, ParseErrorBundle)
import qualified Data.Text.IO as Text (readFile)
import Data.Text (Text)
import Data.Void (Void)
import System.Directory (doesFileExist)
import qualified Text.Read as Text (read)

import Lib

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    it "does not have a test suite" $ do
      compareGoldenFile "test/input/simple.scm" "test/output/simple" >>= assertGoldenResult

assertGoldenResult :: GoldenResult -> IO ()
assertGoldenResult result =
  case result of
    ParseError err -> assertFailure $ show err
    NotEqual _ _ -> assertFailure "NotEqual"
    otherwise -> return ()

type ParseError = ParseErrorBundle Text Void

data GoldenResult =
    ParseError ParseError
  | NotEqual [Sexpr] [Sexpr]
  | Equal
  | Created

compareGoldenFile :: FilePath -> FilePath -> IO GoldenResult
compareGoldenFile inputPath outputPath = do
  input <- Text.readFile inputPath
  case (parse exprs inputPath input) of
    Left err -> return $ ParseError err
    Right expected ->
      readFileIfExists outputPath >>= \case
        Nothing -> do
          writeFile outputPath (show expected)
          return Created
        Just output ->
          let found = read output in
          if expected == found then
            return Equal
          else
            return $ NotEqual expected found
  where
    readFileIfExists :: FilePath -> IO (Maybe String)
    readFileIfExists path = doesFileExist path >>= \case
      True -> Just <$> readFile outputPath
      False -> return Nothing
