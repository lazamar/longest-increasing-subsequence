module Lib
    ( lcs
    , lis
    ) where

import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Longest increasing subsequence
lis :: (Show a, Ord a) => [a] -> [a]
lis = buildResult . snd  . mapAccumL takeMax (Set.empty, Nothing)
    where
        takeMax (endings, lastPreMax) value =
            let
                upperBound = fromMaybe value $ Set.lookupGE value endings
                newMax     = fromMaybe value $ Set.lookupMax newEndings
                newEndings = endings
                    & Set.delete upperBound
                    & Set.insert value
                newPreMax =
                    if upperBound == value
                        then lastPreMax
                        else Set.lookupLT newMax newEndings
            in
            ( (newEndings, newPreMax)
            , (newMax    , newPreMax)
            )

        buildResult [] = []
        buildResult ((max, premax):vals) = takeResult $ foldr f (max:[], premax) (reverse vals)
            where
                f (value, valueNext) (subsequence, next) =
                    if Just value == next
                       then (value:subsequence, valueNext)
                       else (subsequence, next)

                takeResult = fst

lcs :: (Show a, Ord a) => [a] -> [a] -> [a]
lcs rawL1 rawL2 = map fromIndex (lis $ fmap fst $ merged)
    where
        l1 = rawL1
        indexedL2 = reverse $ zip [0..] $ withOccurrences rawL2
        fromIndex idx = rawL2 !! idx
        merged =
            [ (idx, x)
              | y <- l1                 -- For all elements of l1
              , (idx, x) <- indexedL2   -- Get all elements of l2
              , snd x == y                  -- Where their values are the same
            ]

-- | Tag each value with how many times it has appeared in
-- the list so far.
-- O(nlogn)
withOccurrences :: Ord a => [a] -> [(Int, a)]
withOccurrences = snd . mapAccumL f Map.empty
    where
        f acc value = (newAcc, (occurrences, value))
            where
                newAcc      = Map.insertWith (+) value 1 acc
                occurrences = fromJust $ Map.lookup value newAcc

