{- |
The susbet of the entire test-suite which runs in a "speedy" ammount of time.
-}

{-# Language TemplateHaskell #-}
{-# Language Strict #-}

module Test.Speedy.Subset
    ( speedyQuanta
    , speedySubset
    ) where

import Control.Applicative(some, many)
import Data.Functor (void)
import Data.IntSet (IntSet)
import Data.List (uncons)
import GHC.Exts (fromList)
import Language.Haskell.TH.Syntax (Exp(LitE), Lit(StringL), qAddDependentFile, runIO)
import Numeric (readDec)
import Text.ParserCombinators.ReadP (ReadP, char, sepBy, readP_to_S, readS_to_P)


{- |
The number of seconds which an integration test case make take to complete and
still be considered speedy.

Defined as /45 seconds/.
-}
speedyQuanta :: Word
speedyQuanta = 45


{- |
The subset of integration test case which complete within 'speedyQuanta'.
-}
speedySubset :: IntSet
speedySubset =
    let loadedText :: String
        loadedText =
            $(  let dataFile = "data/speedy.txt"
                    embedStr = fmap (LitE . StringL)  . runIO
                in  qAddDependentFile dataFile *> embedStr (readFile dataFile)
            )

        reader :: String -> [Int]
        reader =
           let newline = char '\n'

               dataReader = bodyReader <* footReader

               bodyReader :: ReadP [Int]
               bodyReader = lineReader `sepBy` some newline

               footReader :: ReadP ()
               footReader = void $ many newline

               lineReader :: ReadP Int
               lineReader = readS_to_P readDec

               parseError :: a
               parseError = error "Could not read the \"Speedy Test Case List\"."
               
           in  \str -> maybe parseError fst $ uncons
                    [ x | (x, "") <- readP_to_S dataReader str ]


        staticTestNumbers :: IntSet
        staticTestNumbers = fromList $ reader loadedText
        
    in  $$( [|| staticTestNumbers ||] )


