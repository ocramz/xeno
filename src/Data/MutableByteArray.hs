{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

-- | Unlifted access to ByteArray.

module Data.MutableByteArray
  (MutableByteArray
  ,newMutableByteArray
  ,readIntArray
  ,writeIntArray
  ,unsafeFreezeByteArray
  ,ByteArray
  ,byteArrayToIntVectorDebug)
  where

import           Control.DeepSeq
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal as S
import           Data.ByteString.Unsafe as S
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           GHC.IO
import           GHC.Int
import           GHC.Prim
import           GHC.ST
import           GHC.Word

data MutableByteArray s = MutableByteArray (MutableByteArray# s)

data ByteArray = ByteArray ByteArray#

-- | A helper function, not efficient. For debugging.
byteArrayToIntVectorDebug :: ByteArray -> Int -> Vector Int
byteArrayToIntVectorDebug (ByteArray x) count =
  V.fromList (reverse (go (count - 1)))
  where go 0 = [(I# (indexIntArray# x 0#))]
        go (I# i) = (I# (indexIntArray# x i)) : go ((I# i) - 1)

-- | Create a mutable array of the given size.
newMutableByteArray :: Int -> ST s (MutableByteArray s)
newMutableByteArray (I# size) =
  ST
    (\s ->
       case newByteArray# size s of
         (# s', a #) -> (# s', MutableByteArray a #))
{-# INLINE newMutableByteArray #-}

-- | Read from the array like an integer array.
readIntArray :: MutableByteArray s -> Int -> ST s Int
readIntArray (MutableByteArray a) (I# i) =
  ST (\s -> case readIntArray# a i s of
              (# s', v #) -> (# s', I# v #))
{-# INLINE readIntArray #-}

-- | Write to the array like an integer array.
writeIntArray :: MutableByteArray s -> Int -> Int -> ST s ()
writeIntArray (MutableByteArray a) (I# i) (I# v) =
  ST (\s -> case writeIntArray# a i v s of
              s' -> (# s', () #))
{-# INLINE writeIntArray #-}

-- | Convert a mutable array to a immutable array.
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray a) =
  ST (\s ->
       case unsafeFreezeByteArray# a s of
          (# s, ba #) -> (# s, ByteArray ba #))
{-# INLINE unsafeFreezeByteArray #-}
