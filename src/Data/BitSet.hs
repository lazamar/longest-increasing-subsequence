module Data.BitSet where

import Data.Bits

newtype BitSet a = BitSet Integer
    deriving (Show, Eq)

empty :: BitSet a
empty = BitSet zeroBits

insert :: Enum a => a -> BitSet a -> BitSet a
insert v (BitSet b) = BitSet $ setBit b $ fromEnum v

remove :: Enum a => a -> BitSet a -> BitSet a
remove v (BitSet b) = BitSet $ clearBit b $ fromEnum v

member :: Enum a => a -> BitSet a -> Bool
member v (BitSet b) = testBit b $ fromEnum v

