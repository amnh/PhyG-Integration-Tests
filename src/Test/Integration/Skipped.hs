------------------------------------------------------------------------------
-- |
-- Module      :  Test.Integration.Skipped
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedLists #-}
{-# Language StandaloneDeriving #-}

module Test.Integration.Skipped
    ( -- * Set of integration tests to skip
      SkipSet()
    , saysShouldSkip
    , skipIntegrationTest
      -- * Integration tests with "long" runtimes
    , excessiveRuntime
    ) where


import Control.DeepSeq (NFData)
import Data.Char (isDigit)
import Data.IntSet (IntSet, member, singleton)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import GHC.Generics
import System.FilePath.Posix (takeBaseName)


newtype SkipSet = SkipSet IntSet


deriving newtype  instance Eq SkipSet


deriving stock    instance Generic SkipSet


deriving anyclass instance NFData SkipSet


deriving newtype  instance Ord SkipSet


deriving newtype  instance Semigroup SkipSet


-- |
-- Skip an integration test
skipIntegrationTest :: Enum n => n -> SkipSet 
skipIntegrationTest = SkipSet . singleton . fromEnum


saysShouldSkip :: SkipSet -> FilePath -> Bool
saysShouldSkip (SkipSet set) =
    let getDigits  = reverse . takeWhile isDigit . reverse . takeBaseName
        getSkipped [] = False
        getSkipped ds = read ds `member` set
    in  getSkipped . getDigits

-- |
-- Integration tests with runtimes which exceed 10 seconds.
excessiveRuntime :: SkipSet
excessiveRuntime =
    let skipNumbers :: NonEmpty Word
        skipNumbers =
            [   0
            ,   1
            ,  20
            ,  24
            ,  42
            ,  47
            ,  54
            ,  77
            ,  88
            ,  92
            , 100
            , 115
            , 122
            , 133
            , 137
            , 139
            , 144
            , 155
            , 166
            , 175
            , 179
            , 182
            , 191
            , 195
            , 201
            , 203
            , 208
            , 210
            , 213
            , 214
            , 215
            , 216
            ]
    in  sconcat $ skipIntegrationTest <$> skipNumbers
