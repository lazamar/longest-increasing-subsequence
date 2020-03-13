{-# LANGUAGE Rank2Types #-}

module Lib
    ( lcs
    , lis
    , main
    ) where

import Data.Bifunctor (first, second)
import Data.BitSet as BitSet
import Data.Maybe
import Data.Set (Set)
import Data.Word
import Data.Char
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as B
import qualified Data.Set as Set

main :: IO ()
main = do
    fileName:[] <- getArgs
    content <- B.readFile fileName
    putStrLn $ lis content

-- Longest increasing subsequence
lis :: B.ByteString -> String
lis = fmap chr
    . fromMaybe []
    . fmap trackToList
    . Set.lookupMax
    . fst
    . B.foldl' takeMax (Set.empty, BitSet.empty)

type Accumulator = (Set (Track Int), BitSet)

takeMax :: Accumulator -> Word8 -> Accumulator
takeMax (endings, bitSet) word8value =
    if BitSet.member value bitSet
       then (endings, bitSet)
       else (newEndings, newBitSet)
    where
        value      = fromIntegral word8value
        upperBound = Set.lookupGE (Leaf value) endings
        preSelf    = Set.lookupLT (Leaf value) endings
        self       =
            case preSelf of
                Nothing  -> Leaf value -- Is minimum element
                Just val -> Node value val

        (newEndings, newBitSet)
            = first  (Set.insert self)
            $ second (BitSet.insert value)
            $ case upperBound of
                Nothing -> (endings, bitSet) -- Is maximum element
                Just up ->
                    ( Set.delete up endings
                    , BitSet.remove (thead up) bitSet
                    )

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
