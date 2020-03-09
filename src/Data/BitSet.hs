module Data.BitSet
    ( BitSet
    , empty
    , insert
    , remove
    , member
    ) where

import Data.Bits
import Basement.Types.Word256 (Word256)

newtype BitSet = BitSet Word256

empty :: BitSet
empty = BitSet zeroBits

insert :: Int -> BitSet -> BitSet
insert v (BitSet b) = BitSet $ setBit b v

remove :: Int -> BitSet -> BitSet
remove v (BitSet b) = BitSet $ clearBit b v

member :: Int -> BitSet -> Bool
member v (BitSet b) = testBit b v

