{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | DOM parser and API for XML.

module Xeno.DOM
  ( parse
  , Node
  , Content(..)
  , name
  , attributes
  , contents
  , children
  ) where

import           Control.DeepSeq
import           Control.Monad.ST
import           Control.Spork
import           Data.ByteString          (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (ByteString(PS))
import           Data.Data                (Data, Typeable)
import           Data.Mutable
import           Data.STRef
import           Data.Vector.Unboxed      ((!))
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Xeno.SAX
import           Xeno.Types
import           Xeno.DOM.Internal

-- | Parse a complete Nodes document.
parse :: ByteString -> Either XenoException Node
parse str =
  case spork node of
    Left e -> Left e
    Right r ->
      case findRootNode r of
        Just n -> Right n
        Nothing -> Left XenoExpectRootNode
  where
    findRootNode r = go 0
      where
        go n = case r UV.!? n of
          Just 0x0 -> Just (Node str n r)
          -- skipping text assuming that it contains only white space
          -- characters
          Just 0x1 -> go (n+3)
          _ -> Nothing
    PS _ offset0 _ = str
    node =
      runST
        (do nil <- UMV.new 1000
            vecRef <- newSTRef nil
            sizeRef <- fmap asURef (newRef 0)
            parentRef <- fmap asURef (newRef 0)
            process
              (\(PS _ name_start name_len) -> do
                 let tag = 0x00
                     tag_end = -1
                 index <- readRef sizeRef
                 v' <-
                   do v <- readSTRef vecRef
                      if index + 5 < UMV.length v
                        then pure v
                        else do
                          v' <- UMV.grow v (UMV.length v)
                          writeSTRef vecRef v'
                          return v'
                 tag_parent <- readRef parentRef
                 do writeRef parentRef index
                    writeRef sizeRef (index + 5)
                 do UMV.write v' index tag
                    UMV.write v' (index + 1) tag_parent
                    UMV.write v' (index + 2) (name_start - offset0)
                    UMV.write v' (index + 3) name_len
                    UMV.write v' (index + 4) tag_end)
              (\(PS _ key_start key_len) (PS _ value_start value_len) -> do
                 index <- readRef sizeRef
                 v' <-
                   do v <- readSTRef vecRef
                      if index + 5 < UMV.length v
                        then pure v
                        else do
                          v' <- UMV.grow v (UMV.length v)
                          writeSTRef vecRef v'
                          return v'
                 let tag = 0x02
                 do writeRef sizeRef (index + 5)
                 do UMV.write v' index tag
                    UMV.write v' (index + 1) (key_start - offset0)
                    UMV.write v' (index + 2) key_len
                    UMV.write v' (index + 3) (value_start - offset0)
                    UMV.write v' (index + 4) value_len)
              (\_ -> return ())
              (\(PS _ text_start text_len) -> do
                 let tag = 0x01
                 index <- readRef sizeRef
                 v' <-
                   do v <- readSTRef vecRef
                      if index + 3 < UMV.length v
                        then pure v
                        else do
                          v' <- UMV.grow v (UMV.length v)
                          writeSTRef vecRef v'
                          return v'
                 do writeRef sizeRef (index + 3)
                 do UMV.write v' index tag
                    UMV.write v' (index + 1) (text_start - offset0)
                    UMV.write v' (index + 2) text_len)
              (\_ -> do
                 v <- readSTRef vecRef
                 -- Set the tag_end slot of the parent.
                 parent <- readRef parentRef
                 index <- readRef sizeRef
                 UMV.write v (parent + 4) index
                 -- Pop the stack and return to the parent element.
                 previousParent <- UMV.read v (parent + 1)
                 writeRef parentRef previousParent)
              (\(PS _ cdata_start cdata_len) -> do
                 let tag = 0x03
                 index <- readRef sizeRef
                 v' <-
                   do v <- readSTRef vecRef
                      if index + 3 < UMV.length v
                        then pure v
                        else do
                          v' <- UMV.grow v (UMV.length v)
                          writeSTRef vecRef v'
                          return v'
                 do writeRef sizeRef (index + 3)
                 do UMV.write v' index tag
                    UMV.write v' (index + 1) (cdata_start - offset0)
                    UMV.write v' (index + 2) cdata_len)
              str
            wet <- readSTRef vecRef
            arr <- UV.unsafeFreeze wet
            size <- readRef sizeRef
            return (UV.unsafeSlice 0 size arr))

