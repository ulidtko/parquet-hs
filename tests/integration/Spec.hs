{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Conduit (runResourceT)
import Control.Exception (bracket_)
import Control.Monad.Logger
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Parquet.Reader (readWholeParquetFile)
import Parquet.Prelude
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Process ( callCommand, callProcess )
import Test.Hspec

testPath :: String
testPath = "tests" </> "integration"

testDataPath :: String
testDataPath = testPath </> "testdata"

intermediateDir :: String
intermediateDir = "nested.parquet"

encoderScriptPath :: String
encoderScriptPath = "gen_parquet.py"

outParquetFilePath :: String
outParquetFilePath = testPath </> "test.parquet"

pysparkPythonEnvName :: String
pysparkPythonEnvName = "PYSPARK_PYTHON"

testParquetFormat :: String -> (String -> IO () -> IO ()) -> IO ()
testParquetFormat inputFile performTest =
  bracket_
    (setEnv pysparkPythonEnvName "/usr/bin/python3")
    (unsetEnv pysparkPythonEnvName)
    $ do
      callProcess
        "python3"
        [ testPath </> encoderScriptPath,
          testDataPath </> "input1.json",
          testPath </> intermediateDir
        ]
      callCommand $
        "cp "
          <> testPath
          </> intermediateDir
          </> "*.parquet "
          <> outParquetFilePath

      let close = callProcess "rm" ["-rf", testPath </> intermediateDir]
      performTest outParquetFilePath close
      close

-- callProcess "rm" ["-f", outParquetFilePath]

main :: IO ()
main = hspec $
  describe "Reader" $ do
    it "can read columns" $ do
      testParquetFormat "input1.json" $ \parqFile closePrematurely -> do
        result <-
          runResourceT
            (runStdoutLoggingT (runExceptT (readWholeParquetFile parqFile)))
        case result of
          Left err -> fail $ show err
          Right v -> do
            origJson :: Maybe JSON.Value <- JSON.decode <$> LBS.readFile (testDataPath </> "input1.json")
            closePrematurely
            Just (JSON.encode v) `shouldBe` (JSON.encode <$> origJson)
        pure ()
