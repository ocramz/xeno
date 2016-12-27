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
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Word

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

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+ offset) (S.elemIndex c (S.drop offset str))
-- Without the INLINE below, the whole function is twice as slow and
-- has linear allocation. See git commit with this comment for
-- results.
{-# INLINE elemIndexFrom #-}

-- | Open tag character.
openTagChar :: Word8
openTagChar = 60 -- '<'

-- | Close tag character.
closeTagChar :: Word8
closeTagChar = 62 -- '>'
