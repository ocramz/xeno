{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

-- | Unlifted access to ByteArray.

module Data.ByteString.ByteArray
  (ByteArray(..),toByteArray,toByteArrayIO)
  where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Internal as S
import Data.ByteString.Unsafe as S
import GHC.IO
import GHC.Int
import GHC.Prim
import GHC.Word

-- | A byte array which exposes its unlifted innards for faster
-- manipulation.
data ByteArray = ByteArray ByteArray#
instance NFData ByteArray where
  rnf _ = ()

-- | Convert to a byte array.
toByteArray :: ByteString -> ByteArray
toByteArray !bs = unsafeDupablePerformIO (toByteArrayIO bs)

-- | Convert to a byte array.
toByteArrayIO :: ByteString -> IO ByteArray
toByteArrayIO bs@(PS _ _ (I# len)) =
  IO
    (\state0 ->
       case newByteArray# len state0 of
         (# state, marray #) ->
           let loop i state1 =
                 writeIntArray#
                   marray
                   i
                   (let !(W8# w) = S.unsafeIndex bs (I# i) in word2Int# w)
                   state1
           in case unsafeFreezeByteArray# marray (loop 0# state) of
                (# state', array #) -> (# state', ByteArray array #))

-- parseByteArray :: ByteArray -> ()
-- parseByteArray (ByteArray array) = open 0#
--   where
--     len = sizeofByteArray# array
--     open i =
--       case i <# len of
--         0# -> ()
--         _ ->
--           case indexIntArray# array i of
--             60# -> open (i +# 1#)
--             62# -> open (i +# 1#)
--             _ -> open (i +# 1#)
