{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Test XML parser.

module Xeno (parse) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Word

-- | Parse an XML document.
parse :: ByteString -> ()
parse str =
  parseTags 0
  where
    parseTags index =
      case elemIndexFrom openTagChar str index of
        Nothing ->
          ()
        Just fromLt ->
          case elemIndexFrom closeTagChar str fromLt of
            Nothing -> ()
            Just fromGt -> do
              parseTags (fromGt + 1)

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
