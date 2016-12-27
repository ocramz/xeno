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
      if S.index this 0 == bangChar &&
         S.index this 1 == commentChar && S.index this 2 == commentChar
        then findCommentEnd (index + 3)
        else findTagName index
      where
        this = S.drop index str
    findCommentEnd index =
      case elemIndexFrom commentChar str index of
        Nothing -> error "Couldn't find comment closing '-->' characters."
        Just fromDash ->
          if (S.index this 0 == commentChar)
             -- We could validate the here, but we're a liberal
             -- parser, not a validating parser.
              &&
             (S.index this 1 == closeTagChar)
            then findGt (fromDash + 2)
            else findCommentEnd (fromDash + 1)
          where this = S.drop index str
    findTagName index0 =
      case S.findIndex (not . isTagName) (S.drop index str) of
        Nothing -> error "Couldn't find end of tag name."
        Just ((+ index) -> spaceOrCloseTag) ->
          if S.head this == closeTagChar
            then findGt spaceOrCloseTag
            else if S.head this == spaceChar
                   then findLt spaceOrCloseTag
                   else error
                          ("Expecting space or closing '>' after tag name, but got: " ++
                           show this)
          where this = S.drop spaceOrCloseTag str
      where
        index =
          if S.head (S.drop index0 str) == questionChar ||
             S.head (S.drop index0 str) == slashChar
            then index0 + 1
            else index0
    findLt index =
      case elemIndexFrom closeTagChar str index of
        Nothing -> error "Couldn't find matching '>' character."
        Just fromGt -> do
          findGt (fromGt + 1)

-- | Is the character a valid tag name constituent?
isTagName :: Word8 -> Bool
isTagName c = (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95
{-# INLINE isTagName #-}

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+ offset) (S.elemIndex c (S.drop offset str))
-- Without the INLINE below, the whole function is twice as slow and
-- has linear allocation. See git commit with this comment for
-- results.
{-# INLINE elemIndexFrom #-}

-- | Char for '?'.
questionChar :: Word8
questionChar = 63

-- | Char for '/'.
slashChar :: Word8
slashChar = 47

-- | Character for ' '.
spaceChar :: Word8
spaceChar = 32

-- | Exclaimation character !.
bangChar :: Word8
bangChar = 33

-- | Open tag character.
commentChar :: Word8
commentChar = 45 -- '-'

-- | Open tag character.
openTagChar :: Word8
openTagChar = 60 -- '<'

-- | Close tag character.
closeTagChar :: Word8
closeTagChar = 62 -- '>'
