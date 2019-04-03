{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
module Data.Bitset where

import Data.Bits
import Data.Vector.Unboxed (Vector)
import Data.Word (Word64)

import qualified Data.Vector.Unboxed as Vector

newtype Bitset = Bitset { unBitset :: Vector Word64 }

newtype Union = Union { getUnion :: Bitset }
newtype Intersection = Intersection { getIntersection :: Bitset }

instance Semigroup Union where
  Union a <> Union b = Union $ union a b

instance Monoid Union where
  mempty = Union empty
  mappend = (<>)

instance Semigroup Intersection where
  Intersection a <> Intersection b = Intersection $ intersection a b

-- | @O(1)@
empty :: Bitset
empty = Bitset mempty

singleton :: Int -> Bitset
singleton n =
  Bitset . Vector.generate (q+1) $
  \ix -> if ix == q then shiftL 1 (63 - r) else 0
  where
    (q, r) = divMod n 64

-- | @O(n)@
insert :: Int -> Bitset -> Bitset
insert n (Bitset v) =
  Bitset . Vector.generate (max (q+1) (Vector.length v)) $
  \ix ->
    (if ix == q then shiftL 1 (63 - r) else 0) .|.
    (if ix >= Vector.length v then 0 else v Vector.! ix)
  where
    (q, r) = divMod n 64

-- | @O(1)@
member :: Int -> Bitset -> Bool
member n (Bitset v)
  | q < Vector.length v = shiftL (v Vector.! q) r .&. mask == mask
  | otherwise = False
  where
    mask :: Word64
    mask = 1 `shiftL` 63

    (q, r) = divMod n 64

-- | @O(max(n, m))@
union :: Bitset -> Bitset -> Bitset
union (Bitset v1) (Bitset v2) =
  Bitset . Vector.generate largerLength $
  \ix ->
    if ix >= smallerLength
    then larger Vector.! ix
    else (larger Vector.! ix) .|. (smaller Vector.! ix)
  where
    v1_gt_v2 = Vector.length v1 > Vector.length v2
    larger = if v1_gt_v2 then v1 else v2
    largerLength = Vector.length larger
    smaller = if v1_gt_v2 then v2 else v1
    smallerLength = Vector.length smaller

-- | @O(max(n, m))@
intersection :: Bitset -> Bitset -> Bitset
intersection (Bitset v1) (Bitset v2) =
  Bitset . Vector.generate largerLength $
  \ix ->
    if ix >= smallerLength
    then 0
    else (larger Vector.! ix) .&. (smaller Vector.! ix)
  where
    v1_gt_v2 = Vector.length v1 > Vector.length v2
    larger = if v1_gt_v2 then v1 else v2
    largerLength = Vector.length larger
    smaller = if v1_gt_v2 then v2 else v1
    smallerLength = Vector.length smaller

-- | @O(max(n, m))@
difference :: Bitset -> Bitset -> Bitset
difference (Bitset v1) (Bitset v2) =
  Bitset . Vector.generate (max v1_length v2_length) $
  \ix ->
    if ix >= v1_length
    then 0
    else
      if ix >= v2_length
      then (v1 Vector.! ix)
      else (v1 Vector.! ix) .&. complement (v2 Vector.! ix)
  where
    v1_length = Vector.length v1
    v2_length = Vector.length v2

foldrBitset :: forall b. (Int -> b -> b) -> b -> Bitset -> b
foldrBitset f z = Vector.ifoldr (go 0) z . unBitset
  where
    mask :: Word64
    mask = 1 `shiftL` 63

    go :: Int -> Int -> Word64 -> b -> b
    go !count !ix !w rest
      | count >= 64 = rest
      | otherwise =
        if w .&. mask == mask
        then f (ix*64+count) $ go (count+1) ix (w `shiftL` 1) rest
        else go (count+1) ix (w `shiftL` 1) rest


foldlBitset :: forall b. (b -> Int -> b) -> b -> Bitset -> b
foldlBitset f z = Vector.ifoldl' (go 0) z . unBitset
  where
    mask :: Word64
    mask = 1 `shiftL` 63

    go :: Int -> b -> Int -> Word64 -> b
    go !count !acc !ix !w
      | count >= 64 = acc
      | otherwise =
        if w .&. mask == mask
        then go (count+1) (f acc (ix*64+count)) ix (w `shiftL` 1)
        else go (count+1) acc ix (w `shiftL` 1)
