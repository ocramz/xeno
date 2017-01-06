{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Using the SAX parser, provide a DOM interface.

module Xeno.Vectorize where

import           Control.Monad.ST
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (ByteString(PS))
import           Data.MutableByteArray
import           Data.Vector.Unboxed ((!))
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Xeno

-- | The three bangs below are the diff between 856us and 672us. See commit.
data State s = State
  { stateVec :: !(MutableByteArray s)
  , stateSize :: !Int
  , stateParent :: !Int
  }

parse :: ByteString -> ByteArray -- Vector Int
parse str =
  runST
    (do nil <- newMutableIntArray 1000
        -- trace ("size " ++ show (intArraySize nil)) (return ())
        (State vec size _) <-
          execStateT
            (process
               (\name -> do
                  (State v index tag_parent) <- get
                  let tag = 0x00
                      (name_start, name_end) = byteStringOffset name
                      tag_end = -1
                  v' <-
                    lift
                      (if index + 5 < intArraySize v
                         then pure v
                         else do
                           let newSize = intArraySize v * 2
                           -- trace ("resizing to " ++ show newSize) (return ())
                           resizeMutableByteArray v newSize -- DON'T do this on every push. just do
                       )
                  lift
                    (do writeIntArray v' index tag
                        writeIntArray v' (index + 1) tag_parent
                        writeIntArray v' (index + 2) name_start
                        writeIntArray v' (index + 3) name_end
                        writeIntArray v' (index + 4) tag_end)
                  modify
                    (\s ->
                       s
                       { stateVec = v'
                       , stateParent = index
                       , stateSize = index + 5
                       }))
               (\key value -> do
                  (State v index _) <- get
                  v' <-
                    lift
                      (if index + 5 < intArraySize v
                         then pure v
                         else do
                           let newSize = intArraySize v * 2
                           -- trace ("resizing to " ++ show newSize) (return ())
                           resizeMutableByteArray v newSize -- DON'T do this on every push. just do
                       )
                  let tag = 0x02
                  lift
                    (do let (key_start, key_end) = byteStringOffset key
                            (value_start, value_end) = byteStringOffset value
                        writeIntArray v' index tag
                        writeIntArray v' (index + 1) key_start
                        writeIntArray v' (index + 2) key_end
                        writeIntArray v' (index + 3) value_start
                        writeIntArray v' (index + 4) value_end)
                  modify
                    (\s ->
                       s
                       { stateVec = v'
                       , stateSize = index + 5
                       }))
               (\_name -> return ())
               (\text -> do
                  let tag = 0x01
                      (name_start, name_end) = byteStringOffset text
                  (State v index _) <- get
                  v' <-
                    lift
                      (if index + 5 < intArraySize v
                         then pure v
                         else do
                           let newSize = intArraySize v * 2
                           -- trace ("resizing to " ++ show newSize) (return ())
                           resizeMutableByteArray v newSize -- DON'T do this on every push. just do
                       )
                  lift
                    (do writeIntArray v' (index) tag
                        writeIntArray v' (index + 1) name_start
                        writeIntArray v' (index + 2) name_end)
                  modify (\s -> s {stateVec = v', stateSize = index + 3}))
               (\_ -> do
                  (State vec index parent) <- get
                  -- trace ("[close] Write [" ++ show (parent + 4) ++ "] = " ++ show index) (return ())
                  lift (writeIntArray vec (parent + 4) index)
                  -- trace ("[close] Read [" ++ show (parent + 1) ++ "]") (return ())
                  previousParent <- lift (readIntArray vec (parent + 1))
                  -- trace ("[close] Read: " ++ show previousParent) (return ())
                  setParent previousParent
                  return ())
               str)
            (State nil 0 0)
        arr <- unsafeFreezeByteArray vec
        -- return (byteArrayToIntVectorDebug arr size)
        return arr
        )
  where
    setParent index = modify (\state -> state {stateParent = index})


byteStringOffset :: ByteString -> (Int,Int)
byteStringOffset (PS _ off len) =  (off ,(off+len))
{-# INLINE byteStringOffset #-}

chuck :: ByteString -> Vector Int -> IO ()
chuck original buffer = go 0
  where
    go i =
      if i < V.length buffer
        then case buffer ! i of
               0 ->
                 let parent = buffer ! (i + 1)
                     name_start = buffer ! (i + 2)
                     name_end = buffer ! (i + 3)
                     tag_end = buffer ! (i + 4)
                 in do putStrLn
                         (unlines
                            (zipWith
                               (\i k -> "[" ++ show i ++ "] " ++ k)
                               [i ..]
                               [ "type = tag"
                               , "tag_parent = " ++ show parent
                               , "name_start = " ++ show name_start
                               , "name_end = " ++ show name_end
                               , "tag_end = " ++ show tag_end
                               , "name: " ++ show (substring original name_start name_end)
                               ]))
                       go (i + 5)
               _ ->
                 let text_end = buffer ! (i + 2)
                     text_start = buffer ! (i + 1)
                 in do putStrLn
                         (unlines
                            (zipWith
                               (\i k -> "[" ++ show i ++ "] " ++ k)
                               [i ..]
                               [ "type = text"
                               , "text_start = " ++ show text_start
                               , "text_end = " ++ show text_end
                               , "text: " ++ show (substring original text_start text_end)
                               ]))
                       go (i + 5)
        else return ()

-- {-
-- <r><a>hi</a><b>sup</b>here</r>

-- <  r  >  <  a  >  h  i  <  /  a  >  s  u  p  <  /  b  >  h  e  r  e  <  /  r  >
-- 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30

-- 00
-- type = tag -- <r>
-- tag_parent = -1
-- name_start = ..
-- name_end = 02
-- tag_end = 13
-- 05
-- type = tag -- <a>
-- tag_parent = 00
-- name_start = ..
-- name_end = 05
-- tag_end = 13
-- 10
-- type = text -- "hi"
-- text_start = ...
-- text_end = 08
-- -}

-- | Get a substring of a string.
substring :: ByteString -> Int -> Int -> ByteString
substring s start end = S.take (end - start) (S.drop start s)
{-# INLINE substring #-}
