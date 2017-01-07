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
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (ByteString(PS))
import           Data.IntRef
import           Data.MutableByteArray
import           Data.STRef
import           Data.Vector.Unboxed ((!))
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Xeno

parse :: ByteString -> ByteArray
-- parse :: ByteString -> Vector Int
parse str =
  runST
    (do nil <- newMutableIntArray 1000
        vecRef <- newSTRef nil -- MutVar?
        sizeRef <- newIntRef 0--unbox the I#?
        parentRef <- newIntRef 0
        process
          (\name -> do
             let tag = 0x00
                 (name_start, name_end) = byteStringOffset name
                 tag_end = -1
             index <- readIntRef sizeRef
             v' <-
               do v <- readSTRef vecRef
                  if index + 5 < intArraySize v
                    then pure v
                    else do
                      let newSize = intArraySize v * 2
                      v' <- resizeMutableByteArray v newSize
                      writeSTRef vecRef v'
                      return v'
             do writeIntRef parentRef index
                writeIntRef sizeRef (index + 5)
             do writeIntArray v' index tag
                tag_parent <- readIntRef parentRef
                writeIntArray v' (index + 1) tag_parent
                writeIntArray v' (index + 2) name_start
                writeIntArray v' (index + 3) name_end
                writeIntArray v' (index + 4) tag_end)
          (\key value -> do
             index <- readIntRef sizeRef
             v' <-
               do v <- readSTRef vecRef
                  if index + 5 < intArraySize v
                    then pure v
                    else do
                      let newSize = intArraySize v * 2
                      v' <- resizeMutableByteArray v newSize
                      writeSTRef vecRef v'
                      return v'
             let tag = 0x02
             do writeIntRef sizeRef (index + 5)
             do let (key_start, key_end) = byteStringOffset key
                    (value_start, value_end) = byteStringOffset value
                writeIntArray v' index tag
                writeIntArray v' (index + 1) key_start
                writeIntArray v' (index + 2) key_end
                writeIntArray v' (index + 3) value_start
                writeIntArray v' (index + 4) value_end)
          (\_name -> return ())
          (\text -> do
             let tag = 0x01
                 (name_start, name_end) = byteStringOffset text
             index <- readIntRef sizeRef
             v' <- do
               v <- readSTRef vecRef
               if index + 3 < intArraySize v
                 then pure v
                 else do
                   let newSize = intArraySize v * 2
                   v' <- resizeMutableByteArray v newSize
                   writeSTRef vecRef v'
                   return v'
             do writeIntRef sizeRef (index + 3)
             do writeIntArray v' (index) tag
                writeIntArray v' (index + 1) name_start
                writeIntArray v' (index + 2) name_end)
          (\_ -> do
             index <- readIntRef sizeRef
             v <- readSTRef vecRef
             parent <- readIntRef parentRef
             writeIntArray v (parent + 4) index
             previousParent <- readIntArray v (parent + 1)
             writeIntRef parentRef previousParent)
          str
        wet <- readSTRef vecRef
        arr <- unsafeFreezeByteArray wet
        -- size <- readSTRef sizeRef
        -- return (byteArrayToIntVectorDebug arr size)
        return arr
    )

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
