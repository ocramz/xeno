{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | SAX parser and API for XML.

module Xeno.SAX
  ( process
  , fold
  , validate
  , dump
  ) where

import           Control.Monad.State.Strict
import           Control.Spoon
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Word

--------------------------------------------------------------------------------
-- Helpful interfaces to the parser

-- | Parse the XML but return no result, process no events.
validate :: ByteString -> Bool
validate s =
  case teaspoon
         (runIdentity
            (process
               (\_ -> pure ())
               (\_ _ -> pure ())
               (\_ -> pure ())
               (\_ -> pure ())
               (\_ -> pure ())
               s)) of
    Nothing -> False
    Just () -> True

-- | Parse the XML and pretty print it to stdout.
dump :: ByteString -> IO ()
dump str =
  evalStateT
    (process
       (\name -> do
          level <- get
          lift (S8.putStr (S8.replicate level ' ' <> "<" <> name <> "")))
       (\key value -> lift (S8.putStr (" " <> key <> "=\"" <> value <> "\"")))
       (\_ -> do
          level <- get
          let !level' = level + 2
          put level'
          lift (S8.putStrLn (">")))
       (\text -> do
          level <- get
          lift (S8.putStrLn (S8.replicate level ' ' <> S8.pack (show text))))
       (\name -> do
          level <- get
          let !level' = level - 2
          put level'
          lift (S8.putStrLn (S8.replicate level' ' ' <> "</" <> name <> ">")))
       str)
    (0 :: Int)

-- | Fold over the XML input.
fold
  :: (s -> ByteString -> s) -- ^ Open tag.
  -> (s -> ByteString -> ByteString -> s) -- ^ Attribute key/value.
  -> (s -> ByteString -> s) -- ^ End of open tag.
  -> (s -> ByteString -> s) -- ^ Text.
  -> (s -> ByteString -> s) -- ^ Close tag.
  -> s
  -> ByteString
  -> s
fold openF attrF endOpenF textF closeF s str =
  execState
    (process
       (\name -> modify (\s' -> openF s' name))
       (\key value -> modify (\s' -> attrF s' key value))
       (\name -> modify (\s' -> endOpenF s' name))
       (\text -> modify (\s' -> textF s' text))
       (\name -> modify (\s' -> closeF s' name))
       str)
    s

--------------------------------------------------------------------------------
-- Main parsing function

-- | Process events with callbacks in the XML input.
process
  :: Monad m
  => (ByteString -> m ()) -- ^ Open tag.
  -> (ByteString -> ByteString -> m ()) -- ^ Tag attribute.
  -> (ByteString -> m ()) -- ^ End open tag.
  -> (ByteString -> m ()) -- ^ Text.
  -> (ByteString -> m ()) -- ^ Close tag.
  -> ByteString -> m ()
process openF attrF endOpenF textF closeF str = findLT 0
  where
    findLT index =
      case elemIndexFrom openTagChar str index of
        Nothing -> unless (S.null text) (textF text)
          where text = S.drop index str
        Just fromLt -> do
          unless (S.null text) (textF text)
          checkOpenComment (fromLt + 1)
          where text = substring str index fromLt
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
          if S.index this 0 == commentChar && S.index this 1 == closeTagChar
            then findLT (fromDash + 2)
            else findCommentEnd (fromDash + 1)
          where this = S.drop index str
    findTagName index0 =
      let spaceOrCloseTag = parseName str index
      in if | S.index str index0 == questionChar ->
              case elemIndexFrom closeTagChar str spaceOrCloseTag of
                Nothing -> error "Couldn't find matching '>' character."
                Just fromGt -> do
                  findLT (fromGt + 1)
            | S.index str spaceOrCloseTag == closeTagChar ->
              do let tagname = substring str index spaceOrCloseTag
                 if S.index str index0 == slashChar
                   then closeF tagname
                   else do
                     openF tagname
                     endOpenF tagname
                 findLT (spaceOrCloseTag + 1)
            | otherwise ->
              do let tagname = substring str index spaceOrCloseTag
                 openF tagname
                 result <- findAttributes spaceOrCloseTag
                 endOpenF tagname
                 case result of
                   Right closingTag -> findLT (closingTag + 1)
                   Left closingPair -> do
                     closeF tagname
                     findLT (closingPair + 2)
      where
        index =
          if S.index str index0 == slashChar
            then index0 + 1
            else index0
    findAttributes index0 =
      if S.index str index == slashChar &&
         S.index str (index + 1) == closeTagChar
        then pure (Left index)
        else if S.index str index == closeTagChar
               then pure (Right index)
               else let afterAttrName = parseName str index
                    in if S.index str afterAttrName == equalChar
                         then let quoteIndex = afterAttrName + 1
                                  usedChar = S.index str quoteIndex
                              in if usedChar == quoteChar ||
                                    usedChar == doubleQuoteChar
                                   then case elemIndexFrom
                                               usedChar
                                               str
                                               (quoteIndex + 1) of
                                          Nothing ->
                                            error
                                              "Expecting matching ' or \" to close attribute value."
                                          Just endQuoteIndex -> do
                                            attrF
                                              (substring str index afterAttrName)
                                              (substring
                                                 str
                                                 (quoteIndex + 1)
                                                 (endQuoteIndex))
                                            findAttributes (endQuoteIndex + 1)
                                   else error
                                          "Expecting ' or \" for attribute value, after '='."
                         else error ("Expecting '=' after attribute name, but got: " ++ show (S.take 100 (S.drop afterAttrName str)))
      where
        index = skipSpaces str index0
{-# INLINE process #-}
{-# SPECIALISE process ::
                 (ByteString -> Identity ()) ->
                   (ByteString -> ByteString -> Identity ()) ->
                     (ByteString -> Identity ()) ->
                       (ByteString -> Identity ()) ->
                         (ByteString -> Identity ()) -> ByteString -> Identity ()
               #-}
{-# SPECIALISE process ::
                 (ByteString -> IO ()) ->
                   (ByteString -> ByteString -> IO ()) ->
                     (ByteString -> IO ()) ->
                       (ByteString -> IO ()) ->
                         (ByteString -> IO ()) -> ByteString -> IO ()
               #-}

--------------------------------------------------------------------------------
-- ByteString utilities

-- | A fast space skipping function.
skipSpaces :: ByteString -> Int -> Int
skipSpaces str i =
  if isSpaceChar (S.index str i)
    then skipSpaces str (i + 1)
    else i
{-# INLINE skipSpaces #-}

-- | Get a substring of a string.
substring :: ByteString -> Int -> Int -> ByteString
substring s start end = S.take (end - start) (S.drop start s)
{-# INLINE substring #-}

-- | Basically @findIndex (not . isNameChar)@, but doesn't allocate.
parseName :: ByteString -> Int -> Int
parseName str index =
  if not (isNameChar (S.index str index))
     then index
     else parseName str (index + 1)
{-# INLINE parseName #-}

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+ offset) (S.elemIndex c (S.drop offset str))
-- Without the INLINE below, the whole function is twice as slow and
-- has linear allocation. See git commit with this comment for
-- results.
{-# INLINE elemIndexFrom #-}

--------------------------------------------------------------------------------
-- Character types

isSpaceChar :: Word8 -> Bool
isSpaceChar c = c == 32 || (c <= 10 && c >= 9) || c == 13
{-# INLINE isSpaceChar #-}

-- | Is the character a valid tag name constituent?
isNameChar :: Word8 -> Bool
isNameChar c =
  (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95 || c == 45
{-# INLINE isNameChar #-}

-- | Char for '\''.
quoteChar :: Word8
quoteChar = 39

-- | Char for '"'.
doubleQuoteChar :: Word8
doubleQuoteChar = 34

-- | Char for '='.
equalChar :: Word8
equalChar = 61

-- | Char for '?'.
questionChar :: Word8
questionChar = 63

-- | Char for '/'.
slashChar :: Word8
slashChar = 47

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
