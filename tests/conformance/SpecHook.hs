module SpecHook where

import Parquet.Prelude

import Control.Exception (catch)
import Test.Hspec (describe, it, runIO, expectationFailure, Spec)
import System.Directory (getDirectoryContents)
import System.IO.Error (IOError)


{- | See https://hspec.github.io/hspec-discover.html#spec-hooks

This hook verifies that the test data is there -- i.e. its submodule is checked out.
And clearly reminds to get it, if not.
-}
hook :: Spec -> Spec
hook wrappedSpec = do
  dir <- runIO $ getDirectoryContents "tests/apache-data"
              `catch` \(_ :: IOError) -> return []
  let submoduleThere = "data" `elem` dir && "bad_data" `elem` dir
  if submoduleThere
    then wrappedSpec
    else describe "SUBMODULE MISSING," $
         it "could not find data from repository apache/parquet-testing" $
         expectationFailure "Did you forget to initialize submodules? Try  git submodule update --init"
