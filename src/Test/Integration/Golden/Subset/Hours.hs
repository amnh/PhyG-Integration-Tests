{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
The susbet of the entire test-suite which runs in /"a few hours"/.
-}
module Test.Integration.Golden.Subset.Hours (
    hoursQuanta,
    hoursSubset,
) where

import Data.IntSet (IntSet)
import GHC.Exts (fromList)
import Test.Integration.Golden.Subset.Reader (readDataFileContents, readTestCaseNumbers)


{- |
The number of seconds which an integration test case make take to complete and
still be considered "a few hours."

Defined as /45 seconds/.
-}
hoursQuanta :: Word
hoursQuanta = 45


{- |
The subset of integration test case which complete within 'hoursQuanta'.
-}
hoursSubset :: IntSet
hoursSubset =
    let loadedText :: String
        loadedText = $$(readDataFileContents "data/subset-hours.txt")

        staticTestNumbers :: IntSet
        staticTestNumbers = fromList $ readTestCaseNumbers "hours" loadedText
    in  $$([||staticTestNumbers||])
