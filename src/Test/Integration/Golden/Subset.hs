{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

{- |
The susbet of the entire test-suite which runs in a "rapid" ammount of time.
-}
module Test.Integration.Golden.Subset (
    -- * Subsets
    SubsetRapid (),
    SubsetHours (),
    speedCriteria,
) where

import Data.Char (isDigit)
import Data.IntSet (member)
import Test.Integration.Golden.Subset.Hours (hoursSubset)
import Test.Integration.Golden.Subset.Rapid (rapidSubset)
import Test.Tasty.Options
import Text.Read (readMaybe)


{- |
Option to limit integration test-suite to a subset which completes in /a few hours/.
-}
newtype SubsetHours = SubsetHours Bool


{- |
Option to limit integration test-suite to a subset which completes in /a couple minutes/.
-}
newtype SubsetRapid = SubsetRapid Bool


deriving stock instance Eq SubsetHours


deriving stock instance Eq SubsetRapid


deriving stock instance Ord SubsetHours


deriving stock instance Ord SubsetRapid


deriving stock instance Show SubsetHours


deriving stock instance Show SubsetRapid


instance IsOption SubsetHours where
    defaultValue = SubsetHours False


    parseValue = fmap SubsetHours . safeReadBool


    optionName = pure "subset-hours"


    optionHelp = pure "Run a subset of test cases for a few hours"


    optionCLParser = mkFlagCLParser mempty $ SubsetHours True


instance IsOption SubsetRapid where
    defaultValue = SubsetRapid False


    parseValue = fmap SubsetRapid . safeReadBool


    optionName = pure "subset-rapid"


    optionHelp = pure "Run a rapidly completing subset of test cases"


    optionCLParser = mkFlagCLParser mempty $ SubsetRapid True


{- |
Determine if an individual test case should be included in the test-suite construction
based on the provided speed of completion parameters.
-}
{-# INLINE speedCriteria #-}
speedCriteria ∷ SubsetHours → SubsetRapid → FilePath → Bool
speedCriteria = \case
    SubsetHours True → const isHours
    _ → \case
        SubsetRapid True → isRapid
        _ → const True


{-# INLINEABLE isHours #-}
isHours ∷ FilePath → Bool
isHours =
    let getTestNumber ∷ FilePath → Maybe Int
        getTestNumber = readMaybe . reverse . takeWhile isDigit . reverse

        queryStaticSet ∷ Int → Bool
        queryStaticSet = flip member hoursSubset
    in  maybe False queryStaticSet . getTestNumber


{-# INLINEABLE isRapid #-}
isRapid ∷ FilePath → Bool
isRapid =
    let getTestNumber ∷ FilePath → Maybe Int
        getTestNumber = readMaybe . reverse . takeWhile isDigit . reverse

        queryStaticSet ∷ Int → Bool
        queryStaticSet = flip member rapidSubset
    in  maybe False queryStaticSet . getTestNumber
