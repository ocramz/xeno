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
parse str = findLT 0
  where
    findLT index =
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
             -- We could validate the dashes here, but we're a liberal
             -- parser, not a validating parser.
              &&
             (S.index this 1 == closeTagChar)
            then findLT (fromDash + 2)
            else findCommentEnd (fromDash + 1)
          where this = S.drop index str
    findTagName index0 =
      let spaceOrCloseTag = findEndOfTagName str index
      in if S.index str spaceOrCloseTag == closeTagChar
           then findLT spaceOrCloseTag
           else if S.index str spaceOrCloseTag == spaceChar
                  then case elemIndexFrom closeTagChar str spaceOrCloseTag of
                         Nothing -> error "Couldn't find matching '>' character."
                         Just fromGt -> do
                           findLT (fromGt + 1)
                  else error "Expecting space or closing '>' after tag name."
      where
        index =
          if S.index str index0 == questionChar ||
             S.index str index0 == slashChar
            then index0 + 1
            else index0

-- | Basically @findIndex (not . isTagName)@, but doesn't allocate.
findEndOfTagName :: ByteString -> Int -> Int
findEndOfTagName str index =
  if not (isTagName (S.index str index))
     then index
     else findEndOfTagName str (index + 1)
{-# INLINE findEndOfTagName #-}

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
