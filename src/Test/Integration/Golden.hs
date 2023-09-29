{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Automatic collection and construction of test-suite based on existance of @.golden@ files.
-}
module Test.Integration.Golden (
    collectTestSuite,
) where

import Control.Monad ((<=<))
import Data.Char (isDigit)
import Data.Functor (void)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import System.FilePath.Posix (normalise, stripExtension, takeBaseName, takeDirectory, takeFileName, (<.>), (</>))
import Test.Integration.Golden.Subset (speedCriteria)
import Test.SubProcess
import Test.Tasty (TestTree, askOption, localOption, testGroup, withResource)
import Test.Tasty.Golden (DeleteOutputFile (..), findByExtension, goldenVsFile)
import Text.Read (readMaybe)


data TestCaseComponents = TestCaseComponents
    { subDir :: FilePath
    -- ^ Prefix from the "test case directory"
    , script :: FilePath
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
            compareDigits [] _ = LT
            compareDigits _ [] = GT
            compareDigits as bs = case (readMaybe as, readMaybe bs :: Maybe Word) of
                (Nothing, Nothing) -> EQ
                (Nothing, Just _) -> LT
                (Just _, Nothing) -> GT
                (Just av, Just bv) -> av `compare` bv

            compareChunks x y = case (getOthers x, getOthers y) of
                ((xs, []), (ys, [])) -> xs `compare` ys
                ((_, []), (_, _)) -> LT
                ((_, _), (_, [])) -> GT
                ((xs, a), (ys, b)) -> case xs `compare` ys of
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


{- |
The test-suite for all 'Golden Tests'.

Compares the process's output with a "golden file," the two should be identical.
-}
collectTestSuite :: FilePath -> IO TestTree
collectTestSuite testCaseDir =
    let buildTree :: [TestCaseComponents] -> TestTree
        buildTree components = askOption $ \specHours ->
            askOption $ \specRapid ->
                let speedLimit = speedCriteria specHours specRapid
                in  finalizer $ filter (speedLimit . subDir) components

        -- Do not delete ouput files on test suite success.
        abolisher :: TestTree -> TestTree
        abolisher = localOption Never
--        abolisher = localOption OnPass

        assembler :: TestCaseComponents -> TestTree
        assembler = goldenIntegrationTest testCaseDir

        finalizer :: [TestCaseComponents] -> TestTree
        finalizer = testGroup "Golden Test Cases:" . fmap assembler
    in  abolisher . buildTree <$> collectTestCaseComponents testCaseDir


{- |
Recursively gets all 'scriptExtension' files in a directory.
-}
collectTestCaseComponents :: FilePath -> IO [TestCaseComponents]
collectTestCaseComponents =
    let doffGolden = mapMaybe (stripExtension goldenExtension)
        onlyGolden = [goldenExtension]
        finishCase = uncurry TestCaseComponents
        addOutputs :: (FilePath, FilePath) -> IO TestCaseComponents
        addOutputs v@(path, _) = finishCase v . doffGolden <$> findByExtension onlyGolden path
    in  fmap sort . traverse addOutputs <=< collectScriptContexts


{- |
Recursively search the provided filepath for script files, returning a list of
the directories containing a script file and the name of the script, respectily.
-}
collectScriptContexts :: FilePath -> IO [(FilePath, FilePath)]
collectScriptContexts =
    let scriptContext :: FilePath -> (FilePath, FilePath)
        scriptContext = (,) <$> normalise . takeDirectory <*> takeBaseName
    in  fmap (fmap scriptContext) . findByExtension [scriptExtension]


{- |
Runs a script file [file-name].script producing [file-name].extension for a
given extension which is then compared to [file-name]_extension.golden. If the
golden file does not exist the test will generate it.
-}
goldenIntegrationTest :: FilePath -> TestCaseComponents -> TestTree
goldenIntegrationTest filePath components =
    let -- \| Runs script, ignores resulting output(s).
        generateOutput :: IO ScriptResult
        generateOutput =
            let scriptPath = makePath script components <.> scriptExtension
            in  executeProcess scriptPath

        -- \| Data wrangling functions.
        makeName = takeFileName
        makeOutput = makePath id
        makeGolden = makePath (<.> goldenExtension)

        makePath :: (a -> FilePath) -> a -> FilePath
        makePath f =
            let pref = filePath </> subDir components
            in  normalise . (pref </>) . f

        -- \| Make a Golden Test Case from an output file.
        makeTestCase :: IO ScriptResult -> FilePath -> TestTree
        makeTestCase gen = (goldenVsFile <$> makeName <*> makeGolden <*> makeOutput <*> const (void gen))

        memoizeGeneration :: [FilePath] -> TestTree
        memoizeGeneration fs =
            let bunch :: [TestTree] -> TestTree
                bunch = testGroup $ componentsName components

                -- Don't perform any clean-up.
                clean :: ScriptResult -> IO ()
                clean = const $ pure ()

                setup :: (IO ScriptResult -> TestTree) -> TestTree
                setup = withResource generateOutput clean
            in  setup $ \ioRef -> bunch $ makeTestCase ioRef <$> fs
    in  memoizeGeneration $ outputs components


componentsName :: TestCaseComponents -> String
componentsName c = case subDir c of
    "." -> takeBaseName $ script c
    dir -> takeFileName dir
