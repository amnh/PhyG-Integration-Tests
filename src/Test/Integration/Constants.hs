{- |
Deinitions for locating the target executable to be tested within the integration test suite.
-}

{-# Language DerivingStrategies #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}

module Test.Integration.Constants
    ( binaryName
    , getBinaryUnderTest
    ) where

import Control.Monad ((<=<), filterM)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (isSuffixOf, sort)
import Data.Semigroup (Arg(..))
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Paths_PhyG_integration_tests (getBinDir)
import System.Directory
import System.Exit (die)
import System.FilePath.Posix
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (bindCode, liftTyped, runIO)


{- |
The name of the binary under test
-}
binaryName :: FilePath
binaryName = "phyg"


{- |
Compute the /absolute/ 'FilePath' of the binary under test, from the provided binary file name.

/Note:/

  1.  This 'FilePath' is computed at /compile-time/, by searching within a collection of pre-defined
      directories when Cabal may have placed the binary.

  2.  The supplied binary name must be a /suffix/ of the file name. Suppose within the @.cabal@ file
      there is a @build-tool-depends@ package description field which specifices @$pkgName:$binName@
      as a dependency component. During the build process, it is possible that Cabal will store the
      the dependency component with the file name @$pkgName-$pkgVer$binName@. Hence, to mitigate this
      potential issue, the function checks to ensure that the provided binary name is a suffix of the
      file name (with a possibly empty prefix).

-}
getBinaryUnderTest :: String -> Code Q FilePath
getBinaryUnderTest =
    let exitOnError :: Either String a -> IO a
        exitOnError = either die pure
        getExecutable = exitOnError <=< locateTargetExecutable
        atCompileTime = flip bindCode liftTyped . runIO
    in  atCompileTime . getExecutable


locateTargetExecutable :: String -> IO (Either String FilePath)
locateTargetExecutable inputName =
    let searchLocations :: IO (NonEmpty FilePath)
        searchLocations = getBinDir <&> \dirBinary ->
            let dirCabal = joinPath . init $ splitDirectories dirBinary
            in  dirBinary :| [ dirCabal </> "store" ]

        findMatching :: NonEmpty FilePath -> IO (Maybe (NonEmpty FilePath))
        findMatching =
            let predicate file =
                    let isExecutable = executable <$> getPermissions file
                        isMatching = inputName `isSuffixOf` file
                    in  (&& isMatching) <$> isExecutable
            in  fmap nonEmpty . getFilesFilteredBy predicate

        selectNewest :: Maybe (NonEmpty FilePath) -> IO (Maybe FilePath)
        selectNewest =
            let getLatest :: Ord a => NonEmpty (Arg a FilePath) -> FilePath
                getLatest = (\(Arg _ x) -> x) . maximum
                tagTime f = flip Arg f <$> getModificationTime f
            in  traverse (fmap getLatest . traverse tagTime)

        findBinary :: NonEmpty FilePath -> IO (Maybe FilePath)
        findBinary = selectNewest <=< findMatching

        finalize :: NonEmpty FilePath -> Maybe FilePath -> IO (Either String FilePath)
        finalize dirs =
            let prefix = (">>> " <>)
                indent = ("   - " <>)
                noting key = ((key <> ":") :) . fmap indent

                message :: Either String b
                message = Left . unlines $ prefix <$> fold
                  [ noting "Unable to locate target executable named" [ inputName ]
                  , []
                  , noting "When searching within the directories" $ toList dirs
                  ]

                resultant :: FilePath -> Either a FilePath
                resultant = Right . normalise
            in  traverse makeAbsolute . maybe message resultant

    in  searchLocations >>= \withinDirs ->
            findBinary withinDirs >>= finalize withinDirs


foldMapA :: (FilePath -> IO [FilePath]) -> NonEmpty FilePath -> IO [FilePath]
foldMapA = (fmap fold .) . traverse


{-# INLINE getFilesFilteredBy #-}
-- | Recursively get all files and subdirectories in the given directory that
-- satisfy the given predicate. Note that the content of subdirectories not
-- matching the filter is ignored. In particular, that means something like
-- @getDirFiltered doesFileExist@ will /not/ recursively return all files.
--
-- @since 0.2.2.0
getFilesFilteredBy
  :: (FilePath -> IO Bool) -- ^ File filter
  -> NonEmpty FilePath -- ^ Input paths
  -> IO [FilePath]
getFilesFilteredBy predicate = foldMapA (getFilesFilteredBy' predicate)


{-# INLINE getFilesFilteredBy' #-}
getFilesFilteredBy'
  :: (FilePath -> IO Bool) -- ^ Filepath filter
  -> FilePath
  -> IO [FilePath]
getFilesFilteredBy' check path =
    let canDecend fp = do
            isDir <- doesDirectoryExist fp
            perms <- getPermissions fp
            pure $ isDir && readable perms && searchable perms

        consider fp = liftA2 (&&) (doesFileExist fp) (check fp)

    in  do  all' <- fmap (path </>) . sort <$> listDirectory path
            curr <- filterM consider  all'
            dirs <- filterM canDecend all'
            case nonEmpty dirs of
                Nothing -> pure curr
                Just ds -> do
                    next <- foldMapA (getFilesFilteredBy' check) ds
                    pure $ curr <> next
