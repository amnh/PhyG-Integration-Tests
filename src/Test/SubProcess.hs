{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Spawn a sub-process of the test-suite to run the program under test.
-}
module Test.SubProcess (
    -- * Process types
    ScriptContext (..),
    ScriptResult (..),

    -- * Process Lifecycle
    constructProcess,
    destructProcess,
    executeProcess,

    -- * Other
    binFilePath,
    collectFileContents,
    testCaseDirectory,
) where

import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Monad (when, (>=>))
import Data.ByteString.Lazy (ByteString, readFile)
import Data.Foldable
import Data.Functor (($>))
import System.Directory
import System.Exit (ExitCode)
import System.FilePath.Posix
import System.Process
import Test.Integration.Constants (binaryName, getBinaryUnderTest)
import Prelude hiding (readFile)


{- |
A data structure for storing the process and related paths.
-}
data ScriptContext = ScriptContext
    { process :: CreateProcess
    , outPath :: FilePath
    , errPath :: FilePath
    }


{- |
A data structure for storing the process and related paths.
-}
data ScriptResult = ScriptResult
    { resultingExitCode :: ExitCode
    , resultingLoggedOutput :: FilePath
    , resultingLoggedErrors :: FilePath
    }


{- |
Default directory in which to find the executable for the processes.
-}
binFilePath :: FilePath
binFilePath = $$(getBinaryUnderTest binaryName)


{- |
Default directory in which to find integration tests.
-}
testCaseDirectory :: FilePath
testCaseDirectory = "data"


{- |
Default STDOUT file name.

A process created by this module will not produce any terminal output.
All output will be redirdected to 'outLogFileName' and 'errLogFileName'.
-}
outLogFileName :: FilePath
outLogFileName = "log" <.> "out"


{- |
Default STDERR file name.

All output will be redirdected to 'outLogFileName' and 'errLogFileName'.
-}
errLogFileName :: FilePath
errLogFileName = "log" <.> "err"


{- |
Given a list of 'FilePath's, return a list of the file's contents.
-}
collectFileContents :: (Traversable t) => t FilePath -> IO (t ByteString)
collectFileContents = traverse (nicelyReadFile . (testCaseDirectory </>))
    where
        nicelyReadFile :: FilePath -> IO ByteString
        nicelyReadFile filePath = do
            fileExist <- doesFileExist filePath
            absFilePath <- makeAbsolute filePath
            if fileExist
                then force <$> readFile filePath
                else fail $ unlines ["No file found with the specified filepath:", absFilePath]


{- |
Takes a 'FilePath' to a PCG script and executes an instance of PCG using the
script as the process's input.

Call 'destructProcess' on the supplied 'ScriptContext' afterwards to clean up
artifacts of the process.
-}
constructProcess
    :: FilePath
    -- ^ Relative path to the PCG script
    -> IO ScriptContext
constructProcess scriptStr = do
    prefix <- makeAbsolute scriptDirectory
    let runFilePath = prefix </> scriptFileName
    let outFilePath = prefix </> outLogFileName
    let errFilePath = prefix </> errLogFileName
    let commandStr = unwords [binFilePath, runFilePath, "1>", outFilePath, "2>", errFilePath]

    -- Delete log files if they exist
    _ <- deleteFileIfExists outFilePath
    _ <- deleteFileIfExists errFilePath

    let p =
            CreateProcess
                { cmdspec = ShellCommand commandStr
                , cwd = Just scriptDirectory
                , env = Nothing
                , -- Do not use the stream handles, they do not work with Tasty
                  std_in = NoStream
                , std_out = NoStream
                , std_err = NoStream
                , close_fds = True
                , create_group = False
                , delegate_ctlc = False
                , detach_console = False
                , create_new_console = False
                , new_session = False
                , child_group = Nothing
                , child_user = Nothing
                , use_process_jobs = False
                }

    pure ScriptContext{process = p, outPath = outFilePath, errPath = errFilePath}
    where
        (scriptDirectory, scriptFileName) = breakScriptPath $ testCaseDirectory </> scriptStr

        breakScriptPath = (normalise . foldl' (</>) defaultDirectory . init &&& last) . splitDirectories
            where
                defaultDirectory = "."

        deleteFileIfExists p = do
            fileExists <- doesFileExist p
            when fileExists $ removeFile p


{- |
Clean up after a call to 'constructProcess'.
-}
destructProcess :: ScriptResult -> IO ScriptResult
destructProcess res =
    let -- If no data was written to log file, delete the it!
        cleanLog path = do
            fileExists <- doesFileExist path
            when fileExists $ do
                n <- getFileSize path
                when (n == 0) $ removeFile path
        logPaths = [resultingLoggedOutput, resultingLoggedErrors] <*> [res]
    in  traverse_ cleanLog logPaths $> res


{- |
Runs script on the file passed in.
-}
executeProcess :: FilePath -> IO ScriptResult
executeProcess = constructProcess >=> runSilentProcess >=> destructProcess


runSilentProcess :: ScriptContext -> IO ScriptResult
runSilentProcess ctx =
    let execution :: a -> b -> c -> ProcessHandle -> IO ExitCode
        execution _ _ _ = waitForProcess

        finalizer :: ExitCode -> ScriptResult
        finalizer code = ScriptResult code (outPath ctx) $ errPath ctx
    in  finalizer <$> withCreateProcess (process ctx) execution
