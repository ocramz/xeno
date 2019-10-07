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

import           Control.Monad.ST
import           Control.Spork
import           Data.ByteString          (ByteString)
import           Data.ByteString.Internal (ByteString(PS))
import qualified Data.ByteString as S
import           Data.Mutable
import           Data.STRef
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
          _        -> Nothing
    PS _ offset0 _ = str
    node =
      let !initialSize = max 1000 (S.length str `div` 8) in
      runST
        (do nil <- UMV.unsafeNew initialSize
            vecRef <- newSTRef nil
            sizeRef <- fmap asURef (newRef 0)
            parentRef <- fmap asURef (newRef 0)
            process Process {
                openF = \(PS _ name_start name_len) -> do
                     let tag = 0x00
                         tag_end = -1
                     index <- readRef sizeRef
                     v' <-
                       do v <- readSTRef vecRef
                          if index + 5 < UMV.length v
                            then pure v
                            else do
                              v' <- UMV.unsafeGrow v (predictGrowSize name_start name_len (index + 5) (UMV.length v))
                              writeSTRef vecRef v'
                              return v'
                     tag_parent <- readRef parentRef
                     do writeRef parentRef index
                        writeRef sizeRef (index + 5)
                     do UMV.unsafeWrite v' index tag
                        UMV.unsafeWrite v' (index + 1) tag_parent
                        UMV.unsafeWrite v' (index + 2) (name_start - offset0)
                        UMV.unsafeWrite v' (index + 3) name_len
                        UMV.unsafeWrite v' (index + 4) tag_end
              , attrF = \(PS _ key_start key_len) (PS _ value_start value_len) -> do
                     index <- readRef sizeRef
                     v' <-
                       do v <- readSTRef vecRef
                          if index + 5 < UMV.length v
                            then pure v
                            else do
                              v' <- UMV.unsafeGrow v (predictGrowSize value_start value_len (index + 5) (UMV.length v))
                              writeSTRef vecRef v'
                              return v'
                     let tag = 0x02
                     do writeRef sizeRef (index + 5)
                     do UMV.unsafeWrite v' index tag
                        UMV.unsafeWrite v' (index + 1) (key_start - offset0)
                        UMV.unsafeWrite v' (index + 2) key_len
                        UMV.unsafeWrite v' (index + 3) (value_start - offset0)
                        UMV.unsafeWrite v' (index + 4) value_len
              , endOpenF = \_ -> return ()
              , textF = \(PS _ text_start text_len) -> do
                     let tag = 0x01
                     index <- readRef sizeRef
                     v' <-
                       do v <- readSTRef vecRef
                          if index + 3 < UMV.length v
                            then pure v
                            else do
                              v' <- UMV.unsafeGrow v (predictGrowSize text_start text_len (index + 3) (UMV.length v))
                              writeSTRef vecRef v'
                              return v'
                     do writeRef sizeRef (index + 3)
                     do UMV.unsafeWrite v' index tag
                        UMV.unsafeWrite v' (index + 1) (text_start - offset0)
                        UMV.unsafeWrite v' (index + 2) text_len
              , closeF = \_ -> do
                     v <- readSTRef vecRef
                     -- Set the tag_end slot of the parent.
                     parent <- readRef parentRef
                     index <- readRef sizeRef
                     UMV.unsafeWrite v (parent + 4) index
                     -- Pop the stack and return to the parent element.
                     previousParent <- UMV.unsafeRead v (parent + 1)
                     writeRef parentRef previousParent
              , cdataF = \(PS _ cdata_start cdata_len) -> do
                     let tag = 0x03
                     index <- readRef sizeRef
                     v' <- do
                       v <- readSTRef vecRef
                       if index + 3 < UMV.length v
                         then pure v
                         else do
                           v' <- UMV.unsafeGrow v (predictGrowSize cdata_start cdata_len (index + 3) (UMV.length v))
                           writeSTRef vecRef v'
                           return v'
                     writeRef sizeRef (index + 3)
                     UMV.unsafeWrite v' index tag
                     UMV.unsafeWrite v' (index + 1) (cdata_start - offset0)
                     UMV.unsafeWrite v' (index + 2) cdata_len
              } str
            wet <- readSTRef vecRef
            arr <- UV.unsafeFreeze wet
            size <- readRef sizeRef
            return (UV.unsafeSlice 0 size arr))
            where
                -- Growing a large vector is slow, so we need to do it less times.
                -- We can predict final array size after processing some part (i.e. 1/4) of input XML.
                --
                -- predictGrowSize _bsStart _bsLen _index vecLen = round $ fromIntegral vecLen * (1.25 :: Double)
                predictGrowSize bsStart bsLen index vecLen =
                    let processedLen = bsStart + bsLen - offset0
                        -- 1. Using integral operations, such as
                        --    "predictedTotalSize = (index * S.length str) `div` processedLen"
                        --    cause overflow, so we use float.
                        -- 2. Slightly enlarge predicted size to compensite copy on vector grow
                        --    if prediction is incorrect
                        k = (1.25 :: Double) * fromIntegral (S.length str) / fromIntegral processedLen
                        predictedTotalSize = round $ fromIntegral index * k
                        growSize = predictedTotalSize - vecLen
                    in growSize
