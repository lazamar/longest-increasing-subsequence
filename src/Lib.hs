{-# LANGUAGE Rank2Types #-}

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
lis :: (Show item, Ord item)
    => (forall a. (a -> item -> a) -> a -> container -> a) -- ^ foldl'
    -> container
    -> [item]
lis foldlFun = toResult . foldlFun takeMax Set.empty
    where
        takeMax endings value =
            let
                upperBound = Set.lookupGE (Leaf value) endings
                preSelf    = Set.lookupLT (Leaf value) endings
                self = case preSelf of
                    Nothing  -> Leaf value -- Is minimum element
                    Just val -> Node value val
                newEndings = Set.insert self $
                    case upperBound of
                        Nothing -> endings -- Is maximum element
                        Just up -> Set.delete up endings
            in
            newEndings

        toResult = fromMaybe [] . fmap trackToList . Set.lookupMax

-- Longest common subsequence
lcs :: (Show a, Ord a) => [a] -> [a] -> [a]
lcs l1 l2 = fmap snd $ lis foldl' merged
    where
        byItem          = foldr addToList Map.empty $ zip [1..] l2
        addToList v m   = Map.insertWith (++) (snd v) [v] m
        occurrencesOf v = fromMaybe [] $ Map.lookup v byItem
        merged =
            [ (idx, x)
              | y <- l1                     -- For each elements of l1
              , (idx, x) <- occurrencesOf y -- Get all elements of l2 where their values are the same
            ]

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
