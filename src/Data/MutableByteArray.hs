{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

-- | Unlifted access to ByteArray.

module Data.MutableByteArray
  (MutableByteArray
  ,newMutableIntArray
  ,readIntArray
  ,writeIntArray
  ,resizeMutableByteArray
  ,unsafeFreezeByteArray
  ,ByteArray
  ,byteArrayToIntVectorDebug
  ,intArraySize)
  where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           GHC.Int
import           GHC.Prim
import           GHC.ST

data MutableByteArray s = MutableByteArray (MutableByteArray# s)

data ByteArray = ByteArray ByteArray#

-- | A helper function, not efficient. For debugging.
byteArrayToIntVectorDebug :: ByteArray -> Int -> Vector Int
byteArrayToIntVectorDebug _ count | count < 0 = error "byteArrayToIntVectorDebug: Negative count"
byteArrayToIntVectorDebug (ByteArray x) count =
  V.fromList (reverse (go (count - 1)))
  where go 0 = [(I# (indexIntArray# x 0#))]
        go (I# i) = (I# (indexIntArray# x i)) : go ((I# i) - 1)

-- | Resize the array, with the original contents preserved.
resizeMutableByteArray :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
resizeMutableByteArray _ i | i < 0 = error "resizeMutableByteArray: negative size"
resizeMutableByteArray (MutableByteArray a) (I# size) =
  ST (\s ->
    case resizeMutableByteArray# a (size *# 4#) s of
      (# s', a #) ->
        (# s', MutableByteArray a #))
{-# INLINE resizeMutableByteArray #-}

intArraySize :: MutableByteArray s -> Int
intArraySize (MutableByteArray a) = div (I# (sizeofMutableByteArray# a)) 4
{-# INLINE intArraySize #-}

-- | Create a mutable array of the given size.
newMutableIntArray :: Int -> ST s (MutableByteArray s)
newMutableIntArray i | i < 0 = error "newMutableIntArray: negative size"
newMutableIntArray (I# size) =
  ST
    (\s ->
       case newByteArray# (size *# 4#) s of
         (# s', a #) -> (# s', MutableByteArray  a #))
{-# INLINE newMutableIntArray #-}

-- | Read from the array like an integer array.
readIntArray :: MutableByteArray s -> Int -> ST s Int
readIntArray arr@(MutableByteArray  a) index@(I# i) =
  if index >=0 && index < intArraySize arr
     then ST (\s -> case readInt32Array# a i s of
             (# s', v #) -> (# s', I# v #))
     else error "readIntArray: index out of bounds"
{-# INLINE readIntArray #-}

-- | Write to the array like an integer array.
writeIntArray :: MutableByteArray s -> Int -> Int -> ST s ()
writeIntArray arr@(MutableByteArray  a) index@(I# i) (I# v) =
  if index >=0 && index < intArraySize arr
     then ST (\s -> case writeInt32Array# a i v s of
                      s' -> (# s', () #))
     else error "writeIntArray: index out of bounds"
{-# INLINE writeIntArray #-}

-- | Convert a mutable array to a immutable array.
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray a) =
  ST (\s ->
       case unsafeFreezeByteArray# a s of
          (# s', ba #) -> (# s', ByteArray ba #))
{-# INLINE unsafeFreezeByteArray #-}
