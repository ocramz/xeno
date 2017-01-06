{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

-- | Mutable Int container.

module Data.IntRef where

import Data.MutableByteArray

import           GHC.ST

newtype IntRef s = IntRef (MutableByteArray s)

newIntRef :: Int -> ST s (IntRef s)
newIntRef x =
  fmap
    IntRef
    (do a <- newMutableIntArray 1
        writeIntArray a 0 x
        return a)
{-# INLINE newIntRef #-}

writeIntRef :: IntRef s -> Int -> ST s ()
writeIntRef (IntRef a) v = writeIntArray a 0 v
{-# INLINE writeIntRef #-}

readIntRef :: IntRef s -> ST s Int
readIntRef (IntRef a) = readIntArray a 0
{-# INLINE readIntRef #-}
