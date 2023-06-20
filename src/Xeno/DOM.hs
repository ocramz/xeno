{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP                        #-}
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
#if MIN_VERSION_bytestring(0,11,0)
import           Data.ByteString.Internal (ByteString(BS))
#else
import           Data.ByteString.Internal (ByteString(PS))
#endif
import qualified Data.ByteString as S
import           Data.STRef
import           Data.STRef.Unboxed
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
#if MIN_VERSION_bytestring(0,11,0)
import           Foreign.Ptr (minusPtr)
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import           System.IO.Unsafe (unsafeDupablePerformIO)
#endif
import           Xeno.SAX
import           Xeno.Types
import           Xeno.DOM.Internal

-- | Parse a complete Nodes document.
parse :: ByteString -> Either XenoException Node
parse str =
  maybe (Left XenoExpectRootNode) Right . findRootNode =<< spork node
  where
    findRootNode r = go 0
      where
        go n = case r UV.!? n of
          Just 0x0 -> Just (Node str n r)
          -- skipping text assuming that it contains only white space
          -- characters
          Just 0x1 -> go (n+3)
          _        -> Nothing
#if MIN_VERSION_bytestring(0,11,0)
    BS offset0 _ = str
#else
    PS _ offset0 _ = str
#endif
    node =
      let !initialSize = max 1000 (S.length str `div` 8) in
      runST
        (do nil <- UMV.unsafeNew initialSize
            vecRef <- newSTRef nil
            sizeRef <- newSTRefU 0
            parentRef <- newSTRefU 0
            process Process {
#if MIN_VERSION_bytestring(0,11,0)
                openF = \(BS name_start name_len) -> do
#else
                openF = \(PS _ name_start name_len) -> do
#endif
                     let tag = 0x00
                         tag_end = -1
                     index <- readSTRefU sizeRef
                     v' <-
                       do v <- readSTRef vecRef
                          if index + 5 < UMV.length v
                            then pure v
                            else do
                              v' <- UMV.unsafeGrow v (predictGrowSize name_start name_len (index + 5) (UMV.length v))
                              writeSTRef vecRef v'
                              return v'
                     tag_parent <- readSTRefU parentRef
                     do writeSTRefU parentRef index
                        writeSTRefU sizeRef (index + 5)
                     do UMV.unsafeWrite v' index tag
                        UMV.unsafeWrite v' (index + 1) tag_parent
                        UMV.unsafeWrite v' (index + 2) (distance name_start offset0)
                        UMV.unsafeWrite v' (index + 3) name_len
                        UMV.unsafeWrite v' (index + 4) tag_end
#if MIN_VERSION_bytestring(0,11,0)
              , attrF = \(BS key_start key_len) (BS value_start value_len) -> do
#else
              , attrF = \(PS _ key_start key_len) (PS _ value_start value_len) -> do
#endif
                     index <- readSTRefU sizeRef
                     v' <-
                       do v <- readSTRef vecRef
                          if index + 5 < UMV.length v
                            then pure v
                            else do
                              v' <- UMV.unsafeGrow v (predictGrowSize value_start value_len (index + 5) (UMV.length v))
                              writeSTRef vecRef v'
                              return v'
                     let tag = 0x02
                     do writeSTRefU sizeRef (index + 5)
                     do UMV.unsafeWrite v' index tag
                        UMV.unsafeWrite v' (index + 1) (distance key_start offset0)
                        UMV.unsafeWrite v' (index + 2) key_len
                        UMV.unsafeWrite v' (index + 3) (distance value_start offset0)
                        UMV.unsafeWrite v' (index + 4) value_len
              , endOpenF = \_ -> return ()
#if MIN_VERSION_bytestring(0,11,0)
              , textF = \(BS text_start text_len) -> do
#else
              , textF = \(PS _ text_start text_len) -> do
#endif
                     let tag = 0x01
                     index <- readSTRefU sizeRef
                     v' <-
                       do v <- readSTRef vecRef
                          if index + 3 < UMV.length v
                            then pure v
                            else do
                              v' <- UMV.unsafeGrow v (predictGrowSize text_start text_len (index + 3) (UMV.length v))
                              writeSTRef vecRef v'
                              return v'
                     do writeSTRefU sizeRef (index + 3)
                     do UMV.unsafeWrite v' index tag
                        UMV.unsafeWrite v' (index + 1) (distance text_start offset0)
                        UMV.unsafeWrite v' (index + 2) text_len
              , closeF = \_ -> do
                     v <- readSTRef vecRef
                     -- Set the tag_end slot of the parent.
                     parent <- readSTRefU parentRef
                     index <- readSTRefU sizeRef
                     UMV.unsafeWrite v (parent + 4) index
                     -- Pop the stack and return to the parent element.
                     previousParent <- UMV.unsafeRead v (parent + 1)
                     writeSTRefU parentRef previousParent
#if MIN_VERSION_bytestring(0,11,0)
              , cdataF = \(BS cdata_start cdata_len) -> do
#else
              , cdataF = \(PS _ cdata_start cdata_len) -> do
#endif
                     let tag = 0x03
                     index <- readSTRefU sizeRef
                     v' <- do
                       v <- readSTRef vecRef
                       if index + 3 < UMV.length v
                         then pure v
                         else do
                           v' <- UMV.unsafeGrow v (predictGrowSize cdata_start cdata_len (index + 3) (UMV.length v))
                           writeSTRef vecRef v'
                           return v'
                     writeSTRefU sizeRef (index + 3)
                     UMV.unsafeWrite v' index tag
                     UMV.unsafeWrite v' (index + 1) (distance cdata_start offset0)
                     UMV.unsafeWrite v' (index + 2) cdata_len
              } str
            wet <- readSTRef vecRef
            arr <- UV.unsafeFreeze wet
            size <- readSTRefU sizeRef
            return (UV.unsafeSlice 0 size arr))
            where
                -- Growing a large vector is slow, so we need to do it less times.
                -- We can predict final array size after processing some part (i.e. 1/4) of input XML.
                --
                -- predictGrowSize _bsStart _bsLen _index vecLen = round $ fromIntegral vecLen * (1.25 :: Double)
                predictGrowSize bsStart bsLen index vecLen =
                    let -- at least 1 so we don't divide by zero below and end up with 
                        -- a negative grow size if (bsStart + bsLen - offset0) == 0
                        processedLen = max 1 (distance bsStart offset0 + bsLen)
                        -- 1. Using integral operations, such as
                        --    "predictedTotalSize = (index * S.length str) `div` processedLen"
                        --    cause overflow, so we use float.
                        -- 2. Slightly enlarge predicted size to compensite copy on vector grow
                        --    if prediction is incorrect
                        k = (1.25 :: Double) * fromIntegral (S.length str) / fromIntegral processedLen
                        predictedTotalSize = round $ fromIntegral index * k
                        growSize = predictedTotalSize - vecLen
                    in growSize

#if MIN_VERSION_bytestring(0,11,0)
minusForeignPtr :: ForeignPtr a -> ForeignPtr b -> Int
minusForeignPtr fpA fpB = unsafeDupablePerformIO $
  withForeignPtr fpA $ \ptrA -> withForeignPtr fpB $ \ptrB ->
    pure (minusPtr ptrA ptrB)

distance :: ForeignPtr a -> ForeignPtr b -> Int
distance = minusForeignPtr
#else
distance :: Int -> Int -> Int
distance a b = a - b
#endif
