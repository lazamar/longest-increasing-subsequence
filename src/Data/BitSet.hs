module Data.BitSet
    ( BitSet
    , empty
    , insert
    , remove
    , member
    ) where

import Data.Bits
import Data.Word

data BitSet a = BitSet (a -> Int) Integer

empty :: (a -> Int) -> BitSet a
empty f = BitSet f zeroBits

insert :: a -> BitSet a -> BitSet a
insert v (BitSet f b) = BitSet f $ setBit b $ f v

remove :: a -> BitSet a -> BitSet a
remove v (BitSet f b) = BitSet f $ clearBit b $ f v

member :: a -> BitSet a -> Bool
member v (BitSet f b) = testBit b $ f v

