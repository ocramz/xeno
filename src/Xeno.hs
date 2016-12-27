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
        Just fromLt -> checkOpenComment (fromLt + 1)
    checkOpenComment index =
      if S.isPrefixOf "!--" (S.drop index str)
         then findCommentEnd (index + 3)
         else findLt index
    findCommentEnd index =
      case elemIndexFrom commentChar str index of
        Nothing -> () -- error!
        Just fromDash ->
          if S.isPrefixOf "->" (S.drop (fromDash + 1) str)
             then findGt (fromDash + 2)
             else findCommentEnd (fromDash + 1)
    findLt index =
      case elemIndexFrom closeTagChar str index of
        Nothing -> () -- error!
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
commentChar :: Word8
commentChar = 45 -- '-'

-- | Open tag character.
openTagChar :: Word8
openTagChar = 60 -- '<'

-- | Close tag character.
closeTagChar :: Word8
closeTagChar = 62 -- '>'
