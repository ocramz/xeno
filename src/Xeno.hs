{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Test XML parser.

module Xeno
  ( process
  , fold
  , validate
  , dump
  ) where

import           Control.Monad.State.Strict
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Word

--------------------------------------------------------------------------------
-- Helpful interfaces to the parser

-- | Parse the XML but return no result, process no events.
validate :: ByteString -> ()
validate = runIdentity . process (\_ -> pure ()) (\_ -> pure ()) (\_ -> pure ())

-- | Parse the XML and pretty print it to stdout.
dump :: ByteString -> IO ()
dump str =
  evalStateT
    (process
       (\name -> do
          level <- get
          let !level' = level + 2
          put level'
          lift (S8.putStrLn (S8.replicate level ' ' <> "<" <> name <> ">")))
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
  -> (s -> ByteString -> s) -- ^ Text.
  -> (s -> ByteString -> s) -- ^ Close tag.
  -> s
  -> ByteString
  -> s
fold openF textF closeF s str =
  execState
    (process
       (\name -> modify (\s' -> openF s' name))
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
  -> (ByteString -> m ()) -- ^ Text.
  -> (ByteString -> m ()) -- ^ Close tag.
  -> ByteString -> m ()
process openF textF closeF str = findLT 0
  where
    findLT index =
      case elemIndexFrom openTagChar str index of
        Nothing ->
          if S.null text
            then pure ()
            else textF text
          where text = S.drop index str
        Just fromLt -> do
          if S.null text
            then pure ()
            else textF text
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
      let spaceOrCloseTag = findEndOfTagName str index
      in if S.index str spaceOrCloseTag == closeTagChar
           then do
             let tagname = substring str index spaceOrCloseTag
             if S.index str index0 == questionChar
               then return ()
               else if S.index str index0 == slashChar
                      then closeF tagname
                      else openF tagname
             findLT (spaceOrCloseTag + 1)
           else if S.index str spaceOrCloseTag == spaceChar
                  then case elemIndexFrom closeTagChar str spaceOrCloseTag of
                         Nothing ->
                           error "Couldn't find matching '>' character."
                         Just fromGt -> do
                           let tagname = substring str index spaceOrCloseTag
                           if S.index str index0 == questionChar
                             then return ()
                             else if S.index str index0 == slashChar
                                    then closeF tagname
                                    else openF tagname
                           findLT (fromGt + 1)
                  else error "Expecting space or closing '>' after tag name."
      where
        index =
          if S.index str index0 == questionChar ||
             S.index str index0 == slashChar
            then index0 + 1
            else index0
{-# INLINE process #-}
{-# SPECIALISE process ::
                 (ByteString -> Identity ()) ->
                   (ByteString -> Identity ()) ->
                     (ByteString -> Identity ()) -> ByteString -> Identity ()
               #-}
{-# SPECIALISE process ::
                 (ByteString -> IO ()) ->
                   (ByteString -> IO ()) ->
                     (ByteString -> IO ()) -> ByteString -> IO ()
               #-}

--------------------------------------------------------------------------------
-- ByteString utilities

-- | Get a substring of a string.
substring :: ByteString -> Int -> Int -> ByteString
substring s start end = S.take (end - start) (S.drop start s)
{-# INLINE substring #-}

-- | Basically @findIndex (not . isTagName)@, but doesn't allocate.
findEndOfTagName :: ByteString -> Int -> Int
findEndOfTagName str index =
  if not (isTagName (S.index str index))
     then index
     else findEndOfTagName str (index + 1)
{-# INLINE findEndOfTagName #-}

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+ offset) (S.elemIndex c (S.drop offset str))
-- Without the INLINE below, the whole function is twice as slow and
-- has linear allocation. See git commit with this comment for
-- results.
{-# INLINE elemIndexFrom #-}

--------------------------------------------------------------------------------
-- Character types

-- | Is the character a valid tag name constituent?
isTagName :: Word8 -> Bool
isTagName c = (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95
{-# INLINE isTagName #-}

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
