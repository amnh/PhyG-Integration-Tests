{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
The susbet of the entire test-suite which runs in a "rapid" ammount of time.
-}
module Test.Integration.Golden.Subset.Reader (
    readDataFileContents,
    readTestCaseNumbers,
) where

import Control.Applicative (many)
import Data.Char (isSpace)
import Data.Functor (void)
import Data.IntSet (IntSet)
import Data.List (uncons)
import GHC.Exts (Item)
import Language.Haskell.TH.Syntax (
    Code,
    Exp (LitE),
    Lit (StringL),
    Q,
    bindCode_,
    qAddDependentFile,
    runIO,
    unsafeCodeCoerce,
 )
import Numeric (readDec)
import Text.ParserCombinators.ReadP (ReadP, munch1, readP_to_S, readS_to_P, sepBy)


readDataFileContents ∷ FilePath → Code Q String
readDataFileContents dataFile =
    let embedStr ∷ FilePath → Code Q String
        embedStr = unsafeCodeCoerce . fmap (LitE . StringL) . runIO . readFile
    in  qAddDependentFile dataFile `bindCode_` embedStr dataFile


readTestCaseNumbers ∷ String → String → [Item IntSet]
readTestCaseNumbers name =
    let spacing = munch1 isSpace

        dataReader = bodyReader <* footReader

        bodyReader ∷ ReadP [Item IntSet]
        bodyReader = lineReader `sepBy` spacing

        footReader ∷ ReadP ()
        footReader = void $ many spacing

        lineReader ∷ ReadP (Item IntSet)
        lineReader = readS_to_P readDec

        messageStr ∷ String
        messageStr = "Could not read the \"" <> name <> " Test Case List\".\nReceived:\n\t"

        parseError ∷ [[Item IntSet]] → a
        parseError = error . (messageStr <>) . show
    in  \str →
            let parseResult = [x | (x, "") ← readP_to_S dataReader str]
            in  maybe (parseError parseResult) fst $ uncons parseResult
