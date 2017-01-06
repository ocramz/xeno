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
import           Data.Array.ST
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (ByteString(PS))
-- import           Data.Vector.Unboxed ((!))
-- import           Data.Vector.Unboxed (Vector)
-- import qualified Data.Vector.Unboxed as V
-- import qualified Data.Vector.Unboxed.Mutable as MV
import           Xeno
import qualified Data.Array.MArray as MA
import qualified Data.Array.Unboxed as UA

data State s = State
  { stateVec :: !(STUArray s Int Int)
  , stateSize :: !Int
  , stateParent :: !Int
  }

parse :: ByteString -> UA.UArray Int Int
parse str =
  runSTUArray
    (do nil <- MA.newArray_ (0,2000)
        (State vec size _) <-
          execStateT
            (process
               (\name -> do
                  (State _ index tag_parent) <- get
                  setParent index
                  let tag = 0x00
                      (name_start, name_end) = byteStringOffset name
                      tag_end = -1
                  push tag
                  push tag_parent
                  push name_start
                  push name_end
                  push tag_end)
               (\_key _value -> return ())
               (\_name -> return ())
               (\text -> do
                  let tag = 0x01
                      (name_start, name_end) = byteStringOffset text
                  push tag
                  push name_start
                  push name_end
                  push 0x00
                  push 0x00)
               (\_ -> do
                  (State vec index parent) <- get
                  lift (MA.writeArray vec (parent + 4) index)
                  previousParent <- lift (MA.readArray vec (parent + 1))
                  setParent previousParent)
               str)
            (State nil 0 0)
        return vec)
  where
    setParent index = modify (\state -> state {stateParent = index})
    push x = do
      (State v i parent) <- get
      buf' <-
        lift
        -- v' <-
        --   if i < MV.length v - 1
        --     then pure v
        --     else MV.grow v (MV.length v)-- DON'T do this on
        --                                 -- every push. just do
        --                                 -- it once per node!!!
          (do -- MA.writeArray v i x
              return v)
      let !i' = i + 1
      put (State buf' i' parent) -- calculate this in one go too rather than push push push

byteStringOffset :: ByteString -> (Int,Int)
byteStringOffset (PS _ off len) =  (off ,(off+len))
{-# INLINE byteStringOffset #-}

-- chuck :: ByteString -> Vector Int -> IO ()
-- chuck original buffer = go 0
--   where
--     go i =
--       if i < V.length buffer
--         then case buffer ! i of
--                0 ->
--                  let parent = buffer ! (i + 1)
--                      name_start = buffer ! (i + 2)
--                      name_end = buffer ! (i + 3)
--                      tag_end = buffer ! (i + 4)
--                  in do putStrLn
--                          (unlines
--                             (zipWith
--                                (\i k -> "[" ++ show i ++ "] " ++ k)
--                                [i ..]
--                                [ "type = tag"
--                                , "tag_parent = " ++ show parent
--                                , "name_start = " ++ show name_start
--                                , "name_end = " ++ show name_end
--                                , "tag_end = " ++ show tag_end
--                                , "name: " ++ show (substring original name_start name_end)
--                                ]))
--                        go (i + 5)
--                _ ->
--                  let text_end = buffer ! (i + 2)
--                      text_start = buffer ! (i + 1)
--                  in do putStrLn
--                          (unlines
--                             (zipWith
--                                (\i k -> "[" ++ show i ++ "] " ++ k)
--                                [i ..]
--                                [ "type = text"
--                                , "text_start = " ++ show text_start
--                                , "text_end = " ++ show text_end
--                                , "text: " ++ show (substring original text_start text_end)
--                                ]))
--                        go (i + 5)
--         else return ()

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

-- -- | Get a substring of a string.
-- substring :: ByteString -> Int -> Int -> ByteString
-- substring s start end = S.take (end - start) (S.drop start s)
-- {-# INLINE substring #-}
