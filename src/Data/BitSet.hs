module Data.BitSet
    ( BitSet
    , empty
    , insert
    , remove
    , member
    ) where

import qualified Data.IntSet as IntSet

data BitSet a = BitSet (a -> Int) IntSet.IntSet

empty :: (a -> Int) -> BitSet a
empty f = BitSet f IntSet.empty

insert :: a -> BitSet a -> BitSet a
insert v (BitSet f b) = BitSet f $ IntSet.insert (f v) b

remove :: a -> BitSet a -> BitSet a
remove v (BitSet f b) = BitSet f $ IntSet.delete (f v) b

member :: a -> BitSet a -> Bool
member v (BitSet f b) = IntSet.member (f v) b

