{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Parquet.Prelude hiding (unwords)
import Data.String (unwords)

import Conduit (runResourceT)
import Control.Exception (handle)
import Control.Monad.Logger
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Parquet.Reader (readWholeParquetFile)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.Process (callCommand, callProcess)
import Test.Hspec

testPath :: String
testPath = "tests" </> "integration"

testDataPath :: String
testDataPath = testPath </> "testdata"

intermediateDir :: String
intermediateDir = "nested.parquet"

outParquetFilePath :: String
outParquetFilePath = testPath </> "test.parquet"

testParquetFormat :: String -> (String -> IO ()) -> IO ()
testParquetFormat inputFile performTest = do
  setEnv "PYSPARK_PYTHON" "/usr/bin/python3"
  setEnv "SPARK_LOCAL_IP" "127.0.5.1"
  clear
  callProcess "python3"
    [ testPath </> "gen_parquet.py"
    , testDataPath </> inputFile
    , testPath </> intermediateDir
    ]
  callCommand $ unwords
    [ "cp"
    , testPath </> intermediateDir </> "*.parquet"
    , outParquetFilePath
    ]

  performTest outParquetFilePath
  clear

  where
    clear = callProcess "rm" ["-rf", testPath </> intermediateDir]

main :: IO ()
main = hspec $
  verifyPysparkInstalled $ do
  describe "Reader" $ do
    it "can read columns" $ do
      testParquetFormat "input1.json" $ \parqFile -> do
        result <-
          runResourceT
            (runStdoutLoggingT (runExceptT (readWholeParquetFile parqFile)))
        case result of
          Left err -> fail $ show err
          Right v -> do
            origJson :: Maybe JSON.Value <- JSON.decode <$> LBS.readFile (testDataPath </> "input1.json")
            Just (JSON.encode v) `shouldBe` (JSON.encode <$> origJson)

verifyPysparkInstalled :: Spec -> Spec
verifyPysparkInstalled wrappedSpec =
  runIO detectPyspark >>= \case
    True -> wrappedSpec
    False -> describe "Check dependencies" $
                   it "runs with pyspark" $
                   pendingWith "Test skipped -- no python3 & pyspark detected"
  where
    detectPyspark :: IO Bool
    detectPyspark = handle (\(_ :: SomeException) -> pure False) $
      callProcess "python3" ["-c", "import pyspark"] >> pure True
