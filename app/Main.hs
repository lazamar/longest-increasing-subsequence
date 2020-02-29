module Main where

import Data.Function
import Data.List
import Data.Maybe
import System.Environment
import qualified Data.Set as Set

-- Longest increasing subsequence
lis :: Ord a => [a] -> [a]
lis = buildResult . scanr takeMax (Set.empty, Nothing, Nothing) . reverse
    where
        takeMax value (endings, _, _) =
            let
                upperBound = fromMaybe value  $ Set.lookupGE value endings
                newMax     = fromMaybe value  $ Set.lookupMax newEndings
                preMax     = Set.lookupLT newMax newEndings
                newEndings = endings
                    & Set.delete upperBound
                    & Set.insert value
            in
            (newEndings, Just newMax, preMax)

        buildResult ((_, max, premax):vals) = takeResult $ foldr f (max:[], premax) (reverse vals)
            where
                f (_, value, valueNext) (subsequence, next) =
                    if value == next
                       then (value:subsequence, valueNext)
                       else (subsequence, next)

                takeResult = catMaybes . fst

main :: IO ()
main = do
    val:[] <- getArgs
    print $ lis val

