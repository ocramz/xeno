{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | SAX parser and API for XML.

module Xeno.SAX
  ( process
  , fold
  , validate
  , dump
  ) where

import           Control.Exception
import           Control.Monad.State.Strict
import           Control.Spork
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Unsafe as SU
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Word
import           Xeno.Types

--------------------------------------------------------------------------------
-- Helpful interfaces to the parser

-- | Parse the XML but return no result, process no events.
--
-- N.B.: Only the lexical correctness of the input string is checked, not its XML semantics (e.g. only if tags are well formed, not whether tags are properly closed)
--
-- > > :set -XOverloadedStrings
-- > > validate "<b>"
-- > True
--
-- > > validate "<b"
-- > False
validate :: ByteString -> Bool
validate s =
  case spork
         (runIdentity
            (process
               (\_ -> pure ())
               (\_ _ -> pure ())
               (\_ -> pure ())
               (\_ -> pure ())
               (\_ -> pure ())
               (\_ -> pure ())
               s)) of
    Left (_ :: XenoException) -> False
    Right _ -> True

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
       (\cdata -> do
          level <- get
          lift (S8.putStrLn (S8.replicate level ' ' <> "CDATA: " <> S8.pack (show cdata))))
       str)
    (0 :: Int)

-- | Fold over the XML input.
fold
  :: (s -> ByteString -> s) -- ^ Open tag.
  -> (s -> ByteString -> ByteString -> s) -- ^ Attribute key/value.
  -> (s -> ByteString -> s) -- ^ End of open tag.
  -> (s -> ByteString -> s) -- ^ Text.
  -> (s -> ByteString -> s) -- ^ Close tag.
  -> (s -> ByteString -> s) -- ^ CDATA.
  -> s
  -> ByteString
  -> Either XenoException s
fold openF attrF endOpenF textF closeF cdataF s str =
  spork
    (execState
       (process
          (\name -> modify (\s' -> openF s' name))
          (\key value -> modify (\s' -> attrF s' key value))
          (\name -> modify (\s' -> endOpenF s' name))
          (\text -> modify (\s' -> textF s' text))
          (\name -> modify (\s' -> closeF s' name))
          (\cdata -> modify (\s' -> cdataF s' cdata))
          str)
       s)

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
  -> (ByteString -> m ()) -- ^ CDATA.
  -> ByteString -> m ()
process openF attrF endOpenF textF closeF cdataF str = findLT 0
  where
    findLT index =
      case elemIndexFrom openTagChar str index of
        Nothing -> unless (S.null text) (textF text)
          where text = S.drop index str
        Just fromLt -> do
          unless (S.null text) (textF text)
          checkOpenComment (fromLt + 1)
          where text = substring str index fromLt
    -- Find open comment, CDATA or tag name.
    checkOpenComment index =
      if | s_index this 0 == bangChar -- !
           && s_index this 1 == commentChar -- -
           && s_index this 2 == commentChar -> -- -
           findCommentEnd (index + 3)
         | s_index this 0 == bangChar -- !
           && s_index this 1 == openAngleBracketChar -- [
           && s_index this 2 == 67 -- C
           && s_index this 3 == 68 -- D
           && s_index this 4 == 65 -- A
           && s_index this 5 == 84 -- T
           && s_index this 6 == 65 -- A
           && s_index this 7 == openAngleBracketChar -> -- [
           findCDataEnd (index + 8) (index + 8)
         | otherwise ->
           findTagName index
      where
        this = S.drop index str
    findCommentEnd index =
      case elemIndexFrom commentChar str index of
        Nothing -> throw $ XenoException index str
                         $ XenoParseError "Couldn't find the closing comment dash."
        Just fromDash ->
          if s_index this 0 == commentChar && s_index this 1 == closeTagChar
            then findLT (fromDash + 2)
            else findCommentEnd (fromDash + 1)
          where this = S.drop index str
    findCDataEnd cdata_start index =
      case elemIndexFrom closeAngleBracketChar str index of
        Nothing -> throw $ XenoException index str
                         $ XenoParseError "Couldn't find closing angle bracket for CDATA."
        Just fromCloseAngleBracket ->
          if s_index str (fromCloseAngleBracket + 1) == closeAngleBracketChar
             then do
               cdataF (substring str cdata_start fromCloseAngleBracket)
               findLT (fromCloseAngleBracket + 3) -- Start after ]]>
             else
               -- We only found one ], that means that we need to keep searching.
               findCDataEnd cdata_start (fromCloseAngleBracket + 1)
    findTagName index0 =
      let spaceOrCloseTag = parseName str index
      in if | s_index str index0 == questionChar ->
              case elemIndexFrom closeTagChar str spaceOrCloseTag of
                Nothing -> throw $ XenoException index str
                                 $ XenoParseError "Couldn't find the end of the tag."
                Just fromGt -> do
                  findLT (fromGt + 1)
            | s_index str spaceOrCloseTag == closeTagChar ->
              do let tagname = substring str index spaceOrCloseTag
                 if s_index str index0 == slashChar
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
          if s_index str index0 == slashChar
            then index0 + 1
            else index0
    findAttributes index0 =
      if s_index str index == slashChar &&
         s_index str (index + 1) == closeTagChar
        then pure (Left index)
        else if s_index str index == closeTagChar
               then pure (Right index)
               else let afterAttrName = parseName str index
                    in if s_index str afterAttrName == equalChar
                         then let quoteIndex = afterAttrName + 1
                                  usedChar = s_index str quoteIndex
                              in if usedChar == quoteChar ||
                                    usedChar == doubleQuoteChar
                                   then case elemIndexFrom
                                               usedChar
                                               str
                                               (quoteIndex + 1) of
                                          Nothing ->
                                            throw $ XenoException index str
                                                  $ XenoParseError "Couldn't find the matching quote character."
                                          Just endQuoteIndex -> do
                                            attrF
                                              (substring str index afterAttrName)
                                              (substring
                                                 str
                                                 (quoteIndex + 1)
                                                 (endQuoteIndex))
                                            findAttributes (endQuoteIndex + 1)
                                   else throw $ XenoException index str
                                              $ XenoParseError ("Expected ' or \", got: " <> S.singleton usedChar)
                         else throw $ XenoException index str
                                    $ XenoParseError ("Expected =, got: " <> S.singleton (s_index str afterAttrName) <> " at character index: " <> (S8.pack . show) afterAttrName)
      where
        index = skipSpaces str index0
{-# INLINE process #-}
{-# SPECIALISE process ::
                 (ByteString -> Identity ()) ->
                   (ByteString -> ByteString -> Identity ()) ->
                     (ByteString -> Identity ()) ->
                       (ByteString -> Identity ()) ->
                         (ByteString -> Identity ()) ->
                           (ByteString -> Identity ()) -> ByteString -> Identity ()
               #-}
{-# SPECIALISE process ::
                 (ByteString -> IO ()) ->
                   (ByteString -> ByteString -> IO ()) ->
                     (ByteString -> IO ()) ->
                       (ByteString -> IO ()) ->
                         (ByteString -> IO ()) ->
                           (ByteString -> IO ()) -> ByteString -> IO ()
               #-}

--------------------------------------------------------------------------------
-- ByteString utilities

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
s_index :: ByteString -> Int -> Word8
s_index ps n
    | n < 0            = throw $ XenoException n ps XenoStringIndexProblem
    | n >= S.length ps = throw $ XenoException n ps XenoStringIndexProblem
    | otherwise      = ps `SU.unsafeIndex` n
{-# INLINE s_index #-}

-- | A fast space skipping function.
skipSpaces :: ByteString -> Int -> Int
skipSpaces str i =
  if isSpaceChar (s_index str i)
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
  if not (isNameChar1 (s_index str index))
     then index
     else parseName' str (index + 1)

-- | Basically @findIndex (not . isNameChar)@, but doesn't allocate.
parseName' :: ByteString -> Int -> Int
parseName' str index =
  if not (isNameChar (s_index str index))
     then index
     else parseName' str (index + 1)
{-# INLINE parseName' #-}

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

-- | Is the character a valid first tag/attribute name constituent?
-- 'a'-'z', 'A'-'Z', '_', ':'
isNameChar1 :: Word8 -> Bool
isNameChar1 c =
  (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95 || c == 58
{-# INLINE isNameChar1 #-}

-- | Is the character a valid tag/attribute name constituent?
-- isNameChar1 + '-', '.', '0'-'9'
isNameChar :: Word8 -> Bool
isNameChar c =
  (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95 || c == 58 ||
  c == 45 || c == 46 || (c >= 48 && c <= 57)
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

-- | The dash character.
commentChar :: Word8
commentChar = 45 -- '-'

-- | Open tag character.
openTagChar :: Word8
openTagChar = 60 -- '<'

-- | Close tag character.
closeTagChar :: Word8
closeTagChar = 62 -- '>'

-- | Open angle bracket character.
openAngleBracketChar :: Word8
openAngleBracketChar = 91

-- | Close angle bracket character.
closeAngleBracketChar :: Word8
closeAngleBracketChar = 93
