module Lib
    ( lcs
    , lis
    ) where

import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Longest increasing subsequence
lis :: Ord a => [a] -> [a]
lis = buildResult . snd  . mapAccumL takeMax (Set.empty, Nothing)
    where
        takeMax (endings, lastPreMax) value =
            let
                upperBound = fromMaybe value $ Set.lookupGE value endings
                newMax     = fromMaybe value $ Set.lookupMax newEndings
                newPreMax  = Set.lookupLT newMax newEndings
                newEndings = endings
                    & Set.delete upperBound
                    & Set.insert value
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

-- Longest common subsequence
lcs :: Ord a => [a] -> [a] -> [a]
lcs l1 l2 = map atIndex (lis merged)
    where
        byItem          = foldr addToList Map.empty $ Array.assocs l2Array
        addToList v map = Map.insertWith (++) (snd v) [v] map
        occurrencesOf v = fromMaybe [] $ Map.lookup v byItem

        l2Array         = Array.listArray (0, length l2 - 1) l2
        atIndex idx = l2Array Array.! idx

        merged =
            [ idx
              | y <- l1                     -- For each elements of l1
              , (idx, x) <- occurrencesOf y -- Get all elements of l2
              , x == y                      -- Where their values are the same
            ]
