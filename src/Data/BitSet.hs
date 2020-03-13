{-# LANGUAGE DataKinds #-}

module Data.BitSet
    ( BitSet
    , empty
    , insert
    , remove
    , member
    ) where

--import Data.Bits
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as V (write)

--newtype BitSet = BitSet (MuArray (Offset Bool) (CountOf Bool))
newtype BitSet = BitSet (V.Vector Bool)

empty :: BitSet
empty = BitSet $ V.fromList $ fmap (const False) [1..20000]

insert :: Int -> BitSet -> BitSet
insert v (BitSet b) = BitSet $ setBit True b v

remove :: Int -> BitSet -> BitSet
remove v (BitSet b) = BitSet $ setBit False b v

member :: Int -> BitSet -> Bool
member v (BitSet b) = testBit b v


setBit value vector n = V.modify (\v' -> V.write v' n value) vector
testBit  vector n = vector V.! n
