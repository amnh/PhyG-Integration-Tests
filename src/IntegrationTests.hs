{- |
Top-level entry point for the PhyG Integration test-Suite.
-}
module Main (
    main,
) where

import Data.Proxy (Proxy (..))
import Data.Time.LocalTime (getTimeZone, utcToLocalTime)
import Data.Version (showVersion)
import PackageInfo_PhyG_integration_tests (version)
import Paths_PhyG_integration_tests (getDataDir)
import System.Directory (getModificationTime)
import System.Environment (setEnv)
import System.FilePath ((</>), normalise, splitPath)
import Test.Integration.Golden (collectTestSuite)
import Test.Integration.Golden.Subset (SubsetHours, SubsetRapid)
import Test.SubProcess (binFilePath)
import Test.Tasty
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Options (OptionDescription (Option))


{- |
The entry point for the integration test suite.
-}
main :: IO ()
main = do
    setEnv "TASTY_COLOR" "ALWAYS"
    path <- integrationTestLocation
    integrationTestPreamble path
    testSuiteCompile path >>= defaultMainWithIngredients integrationTestIngredients


{- |
Absolute 'FilePath' where integration test case data is located.
-}
integrationTestLocation :: IO FilePath
integrationTestLocation =
    let testDir = "tests"
        suffixing dPath =
            case reverse $ splitPath dPath of
                [] -> dPath
                x:_ | x == testDir -> dPath
                _ -> dPath </> testDir

    in  suffixing . normalise <$> getDataDir


{- |
Tasty ingredients used for integration test suite.
-}
integrationTestIngredients :: [Ingredient]
integrationTestIngredients =
    let prependItems =
            includingOptions
                [ Option (Proxy :: Proxy SubsetHours)
                , Option (Proxy :: Proxy SubsetRapid)
                ]
        defaultEntry = [rerunningTests defaultIngredients]
    in  prependItems : defaultEntry


{- |
Informational ouput specifying the source of the integration tests
-}
integrationTestPreamble :: FilePath -> IO ()
integrationTestPreamble path =
    let showNicer =
            let tail' :: String -> String
                tail' [] = []
                tail' (_:xs) = xs
            in  reverse . tail' . dropWhile (/= '.') . reverse . show
    in  do
    mTime <- getModificationTime binFilePath
    tZone <- getTimeZone mTime
    let localTime = utcToLocalTime tZone mTime
    putStrLn $
        unlines
            [ "Running integration tests from data source:"
            , "\t" <> path
            , ""
            , "Using the executable:"
            , "\t" <> binFilePath
            , ""
            , "\tBuilt @ " <> showNicer localTime <> " (local time)"
            ]


{- |
Collection of /all/ integration tests.
-}
testSuiteCompile :: FilePath -> IO TestTree
testSuiteCompile = let finalize = testGroup testSuiteName . pure in fmap finalize . collectTestSuite


{- |
Integration test suite name, as displayed in the test log(s).
-}
testSuiteName :: String
testSuiteName = unwords ["Integration", "Test", "Suite", "(", testSuiteVersion, ")"]


{- |
Integration test suite version pretty printed in human readable form.
-}
testSuiteVersion :: String
testSuiteVersion = 'v' : showVersion version
