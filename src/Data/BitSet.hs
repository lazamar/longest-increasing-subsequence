module Data.BitSet
    ( BitSet
    , empty
    , insert
    , remove
    , member
    ) where


import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as V (write)

newtype BitSet = BitSet (V.Vector Bool)

empty :: BitSet
empty = BitSet $ V.fromList $ fmap (const False) [1..20000]

insert :: Int -> BitSet -> BitSet
insert v (BitSet b) = BitSet $ setBit True b v

remove :: Int -> BitSet -> BitSet
remove v (BitSet b) = BitSet $ setBit False b v

{-# INLINE member #-}
member :: Int -> BitSet -> Bool
member v (BitSet b) = b V.! v

setBit :: Bool ->V.Vector Bool -> Int -> V.Vector Bool
setBit value vector n = V.modify (\v' -> V.write v' n value) vector
