{- |
Automatic collection and construction of test-suite based on existance of @.golden@ files.
-}

{-# Language DerivingStrategies #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}

module Test.Integration.Golden
    ( collectTestSuite
    ) where

import Control.Monad (when, (<=<))
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath.Posix
import Test.SubProcess
import Test.Tasty
import Test.Tasty.Golden
import Text.Read (readMaybe)


data  TestCaseComponents
    = TestCaseComponents
    { subDir  :: FilePath
      -- ^ Prefix from the "test case directory"
    , script  :: FilePath
      -- ^ Script file
    , outputs :: [FilePath]
      -- ^ Outputs with the .golden extension
    }


deriving stock instance Eq TestCaseComponents


instance Ord TestCaseComponents where

    compare lhs =
        let getDigits = span isDigit
            getOthers = break isDigit
            compareDigits [] [] = EQ
            compareDigits [] _  = LT
            compareDigits _  [] = GT
            compareDigits as bs = case (readMaybe as, readMaybe bs :: Maybe Word) of
                (Nothing, Nothing) -> EQ
                (Nothing, Just _ ) -> LT
                (Just _ , Nothing) -> GT
                (Just av, Just bv) -> av `compare` bv

            compareChunks x y = case (getOthers x, getOthers y) of
                ((xs, []), (ys, [])) -> xs `compare` ys
                ((_ , []), (_, _)  ) -> LT
                ((_ , _ ), (_, []) ) -> GT
                ((xs, a ), (ys, b) ) -> case xs `compare` ys of
                    v | v /= EQ -> v
                    _ ->
                        let (as, x') = getDigits a
                            (bs, y') = getDigits b
                        in  case compareDigits as bs of
                            EQ -> compareChunks x' y'
                            cv -> cv
        in  compareChunks (subDir lhs) . subDir


type FileExtension = String


goldenExtension :: FileExtension
goldenExtension = ".golden"


scriptExtension :: FileExtension
scriptExtension = ".pg"


-- |
-- The test-suite for all 'Golden Tests'.
--
-- Compares the process's output with a "golden file," the two shpuld be identical.
collectTestSuite :: FilePath -> IO TestTree
collectTestSuite testCaseDir =
    let suiteName = "Golden Test Cases:"
        assembler = goldenIntegrationTest testCaseDir
        finalizer = testGroup suiteName . fmap assembler
    in  finalizer <$> collectTestCaseComponents testCaseDir


-- |
-- Recursively gets all 'scriptExtension' files in a directory.
collectTestCaseComponents :: FilePath -> IO [TestCaseComponents]
collectTestCaseComponents =
    let doffGolden = mapMaybe (stripExtension goldenExtension)
        onlyGolden = [goldenExtension]
        finishCase = uncurry TestCaseComponents
        addOutputs :: (FilePath, FilePath) -> IO TestCaseComponents
        addOutputs v@(path, _) = finishCase v . doffGolden <$> findByExtension onlyGolden path
    in  fmap sort . traverse addOutputs <=< collectScriptContexts


-- |
-- Recursively search the provided filepath for script files, returning a list of
-- the directories containing a script file and the name of the script, respectily.
collectScriptContexts :: FilePath -> IO [(FilePath, FilePath)]
collectScriptContexts =
    let scriptContext :: FilePath -> (FilePath, FilePath)
        scriptContext = (,) <$> normalise . takeDirectory <*> takeBaseName
    in  fmap (fmap scriptContext) . findByExtension [scriptExtension]


-- |
-- Runs a script file [file-name].script producing [file-name].extension for a
-- given extension which is then compared to [file-name]_extension.golden. If the
-- golden file does not exist the test will generate it.
goldenIntegrationTest :: FilePath -> TestCaseComponents -> TestTree
goldenIntegrationTest filePath components =
    let -- | Runs script, ignores resulting output(s).
        generateOutput :: IO ScriptResult
        generateOutput =
            let scriptPath = makePath script components <.> scriptExtension in executeProcess scriptPath

        -- | Data wrangling functions.
        makeName   = takeFileName
        makeOutput = makePath id
        makeGolden = makePath (<.> goldenExtension)

        makePath :: (a -> FilePath) -> a -> FilePath
        makePath f = let pref = filePath </> subDir components in normalise . (pref </>) . f

        -- | Make a Golden Test Case from an output file.
        makeTestCase :: IO ScriptResult -> FilePath -> TestTree
        makeTestCase gen = (goldenVsFile <$> makeName <*> makeGolden <*> makeOutput <*> const (void gen))

        memoizeGeneration :: [FilePath] -> TestTree
        memoizeGeneration fs =
            let clean :: ScriptResult -> IO ()
                clean res = case resultingExitCode res of
                    -- On success purge all artifacts created by script.
                    ExitSuccess   -> traverse_ purge fs
                    -- On failure preserve artifacts for later inspection.
                    ExitFailure{} -> pure ()

                purge :: FilePath -> IO ()
                purge output = case goldenExtension `stripExtension` output of
                    Nothing   -> pure ()
                    Just path -> do
                        fileExists <- doesFileExist path
                        when fileExists $ removeFile path

                setup :: (IO ScriptResult -> TestTree) -> TestTree
                setup = withResource generateOutput clean

                bunch :: [TestTree] -> TestTree
                bunch = testGroup $ componentsName components
            in  setup $ \ioRef -> bunch $ makeTestCase ioRef <$> fs
    in  memoizeGeneration $ outputs components


componentsName :: TestCaseComponents -> String
componentsName c = case subDir c of
    "." -> takeBaseName $ script c
    dir -> takeFileName dir