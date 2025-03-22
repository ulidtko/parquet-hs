{- |
https://github.com/apache/parquet-testing/blob/master/bad_data/README.md
-}
module BadDataSpec (spec) where

import Parquet.Prelude
import Parquet.Reader (readWholeParquetFile)

import Control.Monad.Logger -- (runStdoutLoggingT)
import Control.Exception (throw)
import Control.Monad.Trans.Resource (runResourceT)
import Data.List (isSuffixOf)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents)
import Test.Hspec

badDataDir :: FilePath
badDataDir = "tests/apache-data/bad_data"

spec :: Spec
spec = do
  files <- runIO $ getDirectoryContents badDataDir
                <&> filter (".parquet" `isSuffixOf`)

  describe "apache/parquet-testing \"bad_data\" files" $
    forM_ files $ \f -> do
      it ("should fail reading " ++ f) $ do
        (print =<<) (runT . readWholeParquetFile $ badDataDir </> f) -- hlint bug
          `shouldThrow` anyException

  where runT = either (throw . ParquetExc) return <=<
               runNoLoggingT . runResourceT . runExceptT

newtype ParquetExc = ParquetExc Text deriving Show
instance Exception ParquetExc
