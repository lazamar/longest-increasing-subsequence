module Lib
    ( lcs
    , lis
    ) where

import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
-- Longest increasing subsequence
lis :: (Show a, Ord a) => [a] -> [a]
lis = uncurry buildResult . mapAccumL takeMax (Set.empty, undefined)
    where
        takeMax (endings,_) value =
            let
                upperBound = fromMaybe value $ Set.lookupGE value endings
                newMax     = fromMaybe value $ Set.lookupMax newEndings
                newPreMax  = Set.lookupLT value newEndings
                newEndings = endings
                    & Set.delete upperBound
                    & Set.insert value
            in
            ( (newEndings, newMax   )
            , (value     , newPreMax)
            )

        buildResult _ [] = []
        buildResult (_, maxv) vals = takeResult $ foldr f ([], Just maxv) vals
            where
                f (value, valueNext) (subsequence, next) =
                    if Just value == next
                       then (value:subsequence, valueNext)
                       else (subsequence, next)

                takeResult = fst

-- Longest common subsequence
lcs :: (Show a, Ord a) => [a] -> [a] -> [a]
lcs l1 l2 = fmap snd $ lis merged
    where
        byItem          = foldr addToList Map.empty $ zip [1..] l2
        addToList v m   = Map.insertWith (++) (snd v) [v] m
        occurrencesOf v = fromMaybe [] $ Map.lookup v byItem
        merged =
            [ (idx, x)
              | y <- l1                     -- For each elements of l1
              , (idx, x) <- occurrencesOf y -- Get all elements of l2 where their values are the same
            ]
