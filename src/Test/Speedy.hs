{- |
The susbet of the entire test-suite which runs in a "speedy" ammount of time.
-}

{-# Language DerivingStrategies #-}
{-# Language Strict #-}
{-# Language StandaloneDeriving #-}

module Test.Speedy
    ( SpeedySubset()
    , speedCriteria
    ) where

import Data.Char (isDigit)
import Data.IntSet (member)
import Test.Speedy.Subset
import Test.Tasty.Options
import Text.Read (readMaybe)


newtype SpeedySubset = SpeedySubset Bool


deriving stock instance Eq SpeedySubset


deriving stock instance Ord SpeedySubset


instance IsOption SpeedySubset where

    defaultValue = SpeedySubset False
    
    parseValue = fmap SpeedySubset . safeReadBool

    optionName = pure "speedy-subset"

    optionHelp = pure "Run a quickly completing subset of Integration Test-Suite"

    optionCLParser = mkFlagCLParser mempty $ SpeedySubset False


{-# INLINE speedCriteria #-}
speedCriteria :: SpeedySubset -> FilePath -> Bool
speedCriteria (SpeedySubset quick)
    | not quick = const True
    | otherwise = isSpeedy


{-# INLINABLE isSpeedy #-}
isSpeedy :: FilePath -> Bool
isSpeedy =
    let getTestNumber :: FilePath -> Maybe Int
        getTestNumber = readMaybe . reverse . takeWhile isDigit . reverse

        queryStaticSet :: Int -> Bool
        queryStaticSet = flip member speedySubset
        
    in  maybe False queryStaticSet . getTestNumber
