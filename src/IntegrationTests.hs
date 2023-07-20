{- |
Top-level entry point for the PhyG Integration test-Suite.
-}

module Main
    ( main
    ) where

import Data.Version (showVersion)
import PackageInfo_PhyG_integration_tests (version)
import Paths_PhyG_integration_tests (getDataDir)
import System.FilePath (normalise)
import Test.Integration.Golden (collectTestSuite)
import Test.SubProcess (binFilePath)
import Test.Tasty
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Rerun (rerunningTests)


-- |
-- The entry point for the integration test suite.
main :: IO ()
main = do
    path <- integrationTestLocation
    integrationTestPreamble path
    testSuiteCompile path >>= defaultMainWithIngredients integrationTestIngredients


-- |
-- Absolute 'FilePath' where integration test case data is located.
integrationTestLocation :: IO FilePath
integrationTestLocation = normalise <$> getDataDir


-- |
-- Tasty ingredients used for integration test suite.
integrationTestIngredients :: [Ingredient]
integrationTestIngredients = [rerunningTests defaultIngredients]


-- |
-- Informational ouput specifying the source of the integration tests
integrationTestPreamble :: FilePath -> IO ()
integrationTestPreamble path = putStrLn $ unlines
    [ "Running integration tests from data source:"
    , "\t" <> path
    , ""
    , "Using the executable:"
    , "\t" <> binFilePath
    ]


-- |
-- Collection of /all/ integration tests.
testSuiteCompile :: FilePath -> IO TestTree
testSuiteCompile = let finalize = testGroup testSuiteName . pure in fmap finalize . collectTestSuite


-- |
-- Integration test suite name, as displayed in the test log(s).
testSuiteName :: String
testSuiteName = unwords ["Integration", "Test", "Suite", "(", testSuiteVersion, ")"]


-- |
-- Integration test suite version pretty printed in human readable form.
testSuiteVersion :: String
testSuiteVersion = 'v' : showVersion version
