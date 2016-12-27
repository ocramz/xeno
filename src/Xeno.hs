{-# LANGUAGE Unsafe #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Test XML parser.

module Xeno
  ( parse
  , parseByteArray
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.ByteArray
import           Data.Word
import           GHC.Prim

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+ offset) (S.elemIndex c (S.drop offset str))
{-# INLINE elemIndexFrom #-}

-- | Open tag character.
openTagChar :: Word8
openTagChar = 60 -- '<'

-- | Close tag character.
closeTagChar :: Word8
closeTagChar = 62 -- '>'

-- | Naive version with ByteString.
parse :: ByteString -> ()
parse str = findGt 0
  where
    findGt index =
      case elemIndexFrom openTagChar str index of
        Nothing -> ()
        Just fromLt -> findLt fromLt
    findLt index =
      case elemIndexFrom closeTagChar str index of
        Nothing -> ()
        Just fromGt -> do
          findGt fromGt

parseByteArray :: ByteArray -> ()
parseByteArray (ByteArray array) = open 0#
  where
    len = sizeofByteArray# array
    open i =
      case i <# len of
        0# -> ()
        _ ->
          case indexIntArray# array i of
            60# -> open (i +# 1#)
            62# -> open (i +# 1#)
            _ -> open (i +# 1#)
