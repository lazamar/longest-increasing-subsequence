{-# LANGUAGE Rank2Types #-}

module Lib
    ( lcs
    , lis
    ) where

import Control.Arrow ((***))
import Data.Function
import Data.List
import Data.Maybe
import Data.BitSet as BitSet
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Longest increasing subsequence
lis :: (Ord item)
    => (item -> Int) -- ^ Must be injective
    -> (forall a.  (a -> item -> a) -> a -> container -> a) -- ^ foldl'
    -> container
    -> [Int]
lis toInt foldlFun = toResult . foldlFun (\t v -> takeMax t $ toInt v) (Set.empty, BitSet.empty)
    where
        takeMax (endings, bitSet) value =
            if BitSet.member value bitSet
               then (endings, bitSet)
               else
                    let upperBound = Set.lookupGE (Leaf value) endings
                        preSelf    = Set.lookupLT (Leaf value) endings
                        self = case preSelf of
                            Nothing  -> Leaf value -- Is minimum element
                            Just val -> Node value val

                        (newEndings, newBitSet)
                            = (Set.insert self *** BitSet.insert value)
                            $ case upperBound of
                                Nothing -> (endings, bitSet) -- Is maximum element
                                Just up ->
                                    ( Set.delete up endings
                                    , BitSet.remove (thead up) bitSet
                                    )
                    in
                    (newEndings, newBitSet)

        toResult = fromMaybe [] . fmap trackToList . Set.lookupMax . fst

-- Longest common subsequence
lcs :: (Ord a) => [a] -> [a] -> [a]
lcs = undefined
-- lcs l1 l2 = fmap snd $ lis (fromIntegral . fst) foldl' merged
--      where
--          byItem          = foldr addToList Map.empty $ zip [1..] l2
--          addToList v m   = Map.insertWith (++) (snd v) [v] m
--          occurrencesOf v = fromMaybe [] $ Map.lookup v byItem
--          merged =
--              [ (idx, x)
--                | y <- l1                     -- For each elements of l1
--                , (idx, x) <- occurrencesOf y -- Get all elements of l2 where their values are the same
--              ]

data Track a
  = Leaf a
  | Node a (Track a)

thead :: Track a -> a
thead (Leaf a) = a
thead (Node a _) = a

trackToList :: Track a -> [a]
trackToList = go []
    where
        go acc (Leaf a) = a:acc
        go acc (Node a track) = go (a:acc) track

instance Eq a => Eq (Track a) where
    t1 == t2 = thead t1 == thead t2

instance Ord a => Ord (Track a) where
    t1 `compare` t2 = thead t1 `compare` thead t2
