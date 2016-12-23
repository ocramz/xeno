{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Test XML parser.

module Xeno
  ( parse
  , parseErikd
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as BSI
import           Data.Word
import qualified Foreign.ForeignPtr as FP
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified System.IO.Unsafe as U

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

-- | ErikD's contribution.
parseErikd :: ByteString -> ()
parseErikd (BSI.PS fptr offset len) =
  U.unsafePerformIO . FP.withForeignPtr fptr $ \ srcptr ->
    let ptr = FP.plusPtr srcptr offset
    in parseTags ptr 0
  where
    parseTags :: FP.Ptr Word8 -> Int -> IO ()
    parseTags ptr index
      | index >= len = pure ()
      | otherwise = do
          el <- FS.peekElemOff ptr index
          case el of
            60 {- '<' -} -> pure ()
            62 {- '>' -} -> pure ()
            _ -> pure ()
          parseTags ptr (index + 1)
