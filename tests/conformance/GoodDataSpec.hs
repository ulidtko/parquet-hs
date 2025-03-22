{- |
https://github.com/apache/parquet-testing/blob/master/data/README.md
-}
module GoodDataSpec (spec) where

import Parquet.Prelude
import Parquet.Reader (readWholeParquetFile)

import Control.Monad.Logger -- (runStdoutLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.List (isSuffixOf)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents)
import Test.Hspec

goodDataDir :: FilePath
goodDataDir = "tests/apache-data/data"

spec :: Spec
spec = do
  files <- runIO $ getDirectoryContents goodDataDir
                <&> filter (".parquet" `isSuffixOf`)

  describe "apache/parquet-testing \"good data\" files" $
    forM_ files $ \f -> do
      it ("should succeed reading " ++ f) $ do
        res <- runT . readWholeParquetFile $ goodDataDir </> f
        res `shouldSatisfy` isRight

  where runT = runNoLoggingT . runResourceT . runExceptT
