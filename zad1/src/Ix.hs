{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
module Ix where

class Ord a => Ix a where
    range :: (a, a) -> [a]
    default range :: Enum a => (a, a) -> [a]
    range = uncurry enumFromTo
    {-# INLINE range #-}

    index :: (a, a) -> a -> Int
    index r ix
        | inRange r ix = unsafeIndex r ix
        | otherwise = error "Out of bounds"
    {-# INLINE index #-}

    unsafeIndex :: (a, a) -> a -> Int
    default unsafeIndex :: Enum a => (a, a) -> a -> Int
    unsafeIndex (begin, _) ix = fromEnum ix - fromEnum begin
    {-# INLINE unsafeIndex #-}

    inRange :: (a, a) -> a -> Bool
    default inRange :: Enum a => (a, a) -> a -> Bool
    inRange (fromEnum -> begin, fromEnum -> end) (fromEnum -> ix) = begin <= ix && ix <= end
    {-# INLINE inRange #-}

    rangeSize :: (a, a) -> Int
    default rangeSize :: Enum a => (a, a) -> Int
    rangeSize (begin, end)
        | begin <= end = 1 + fromEnum end - fromEnum begin
        | otherwise = 0
    {-# INLINE rangeSize #-}

instance Ix Char

instance Ix Int

instance Ix Integer where
    unsafeIndex (begin, _) ix = fromIntegral $ ix - begin
    {-# INLINE unsafeIndex #-}

    inRange (begin, end) ix = begin <= ix && ix <= end
    {-# INLINE inRange #-}

    rangeSize (begin, end)
        | begin <= end = fromInteger $ end - begin + 1
        | otherwise = 0
    {-# INLINE rangeSize #-}

instance (Ix a, Ix b) => Ix (a, b) where
    range ((b0, b1), (e0, e1)) = (,) <$> range (b0, e0) <*> range (b1, e1)
    {-# INLINE range #-}

    unsafeIndex ((b0, b1), (e0, e1)) (i0, i1) = rangeSize (b1, e1) * index (b0, e0) i0 + index (b1, e1) i1
    {-# INLINE unsafeIndex #-}

    inRange ((b0, b1), (e0, e1)) (i0, i1) = inRange (b0, e0) i0 && inRange (b1, e1) i1
    {-# INLINE inRange #-}

    rangeSize ((b0, b1), (e0, e1)) = rangeSize (b0, e0) * rangeSize (b1, e1)
    {-# INLINE rangeSize #-}
