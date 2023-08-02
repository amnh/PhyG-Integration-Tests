{- |
The susbet of the entire test-suite which runs in a "rapid" ammount of time.
-}

{-# Language TemplateHaskell #-}
{-# Language Strict #-}

module Test.Integration.Golden.Subset.Rapid
    ( rapidQuanta
    , rapidSubset
    ) where

import Data.IntSet (IntSet)
import GHC.Exts (fromList)
import Test.Integration.Golden.Subset.Reader (readDataFileContents, readTestCaseNumbers)


{- |
The number of seconds which an integration test case make take to complete and
still be considered rapid.

Defined as /3 seconds/.
-}
rapidQuanta :: Word
rapidQuanta = 3


{- |
The subset of integration test case which complete within 'rapidQuanta'.
-}
rapidSubset :: IntSet
rapidSubset =
    let loadedText :: String
        loadedText = $$( readDataFileContents "data/subset-rapid.txt" )

        staticTestNumbers :: IntSet
        staticTestNumbers = fromList $ readTestCaseNumbers "rapid" loadedText

    in  $$( [|| staticTestNumbers ||] )
