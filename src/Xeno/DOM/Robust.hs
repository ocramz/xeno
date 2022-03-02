{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP                        #-}
-- | DOM parser and API for XML.
--   Slightly slower DOM parsing,
--   but add missing close tags.
module Xeno.DOM.Robust
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
#if MIN_VERSION_bytestring(0,11,0)
import           Data.ByteString.Internal as BS (ByteString(..), plusForeignPtr)
#else
import           Data.ByteString.Internal(ByteString(..))
#endif
import           Data.STRef
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Data.Mutable(asURef, newRef, readRef, writeRef)
#if MIN_VERSION_bytestring(0,11,0)
import           Foreign.Ptr (minusPtr)
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import           System.IO.Unsafe (unsafeDupablePerformIO)
#endif
import           Xeno.SAX
import           Xeno.Types
import           Xeno.DOM.Internal(Node(..), Content(..), name, attributes, contents, children)

-- | Parse a complete Nodes document.
parse :: ByteString -> Either XenoException Node
parse inp =
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
#if MIN_VERSION_bytestring(0,11,0)
    BS offset0 _ = str
#else
    PS _ offset0 _ = str
#endif
    str = skipDoctype inp
    node =
      runST
        (do nil <- UMV.new 1000
            vecRef    <- newSTRef nil
            sizeRef   <- fmap asURef $ newRef 0
            parentRef <- fmap asURef $ newRef 0
            process Process {
#if MIN_VERSION_bytestring(0,11,0)
                openF = \(BS name_start name_len) -> do
#else
                openF = \(PS _ name_start name_len) -> do
#endif
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
                    UMV.write v' index tag
                    UMV.write v' (index + 1) tag_parent
                    UMV.write v' (index + 2) (distance name_start offset0)
                    UMV.write v' (index + 3) name_len
                    UMV.write v' (index + 4) tag_end
#if MIN_VERSION_bytestring(0,11,0)
              , attrF = \(BS key_start key_len) (BS value_start value_len) -> do
#else
              , attrF = \(PS _ key_start key_len) (PS _ value_start value_len) -> do
#endif
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
                    UMV.write v' (index + 1) (distance key_start offset0)
                    UMV.write v' (index + 2) key_len
                    UMV.write v' (index + 3) (distance value_start offset0)
                    UMV.write v' (index + 4) value_len
              , endOpenF = \_ -> return ()
#if MIN_VERSION_bytestring(0,11,0)
              , textF = \(BS text_start text_len) -> do
#else
              , textF = \(PS _ text_start text_len) -> do
#endif
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
                    UMV.write v' (index + 1) (distance text_start offset0)
                    UMV.write v' (index + 2) text_len
#if MIN_VERSION_bytestring(0,11,0)
              , closeF = \closeTag@(BS _ _) -> do
#else
              , closeF = \closeTag@(PS s _ _) -> do
#endif
                 v <- readSTRef vecRef
                 -- Set the tag_end slot of the parent.
                 index <- readRef sizeRef
                 untilM $ do
                   parent <- readRef parentRef
                   correctTag <- if parent == 0
                                    then return True -- no more tags to close!!!
                                    else do
                                      parent_name <- UMV.read v (parent + 2)
                                      parent_len  <- UMV.read v (parent + 3)
#if MIN_VERSION_bytestring(0,11,0)
                                      let openTag  = BS (BS.plusForeignPtr offset0 parent_name) parent_len
#else
                                      let openTag  = PS s (parent_name+offset0) parent_len
#endif
                                      return       $ openTag == closeTag
                   UMV.write                  v (parent + 4) index
                   -- Pop the stack and return to the parent element.
                   previousParent <- UMV.read v (parent + 1)
                   writeRef parentRef previousParent
                   return correctTag -- continue closing tags, until matching one is found
#if MIN_VERSION_bytestring(0,11,0)
              , cdataF = \(BS cdata_start cdata_len) -> do
#else
              , cdataF = \(PS _ cdata_start cdata_len) -> do
#endif
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
                    UMV.write v' (index + 1) (distance cdata_start offset0)
                    UMV.write v' (index + 2) cdata_len
              } str
            wet <- readSTRef vecRef
            arr <- UV.unsafeFreeze wet
            size <- readRef sizeRef
            return (UV.unsafeSlice 0 size arr))

untilM :: Monad m => m Bool -> m ()
untilM loop = do
  cond <- loop
  case cond of
    True  -> return ()
    False -> untilM loop

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
