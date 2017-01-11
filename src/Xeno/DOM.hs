{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (ByteString(PS))
import           Data.Mutable
import           Data.STRef
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Xeno.SAX

-- | Some XML nodes.
data Node = Node !ByteString !Int !(UV.Vector Int)
  deriving (Eq)

instance NFData Node where
  rnf !_ = ()

instance Show Node where
  show n =
    "(Node " ++
    show (name n) ++
    " " ++ show (attributes n) ++ " " ++ show (contents n) ++ ")"

-- | Content of a node.
data Content
  = Element {-# UNPACK #-}!Node
  | Text {-# UNPACK #-}!ByteString
  deriving (Show)

-- | Get just element children of the node (no text).
children :: Node -> [Node]
children (Node str start offsets) = collect firstChild
  where
    collect i
      | i < endBoundary =
        case offsets ! i of
          0x00 -> Node str i offsets : collect (offsets ! (i + 4))
          0x01 -> collect (i + 3)
          _ -> []
      | otherwise = []
    firstChild = go (start + 5)
      where
        go i
          | i < endBoundary =
            case offsets ! i of
              0x02 -> go (i + 5)
              _ -> i
          | otherwise = i
    endBoundary = offsets ! (start + 4)

-- | Contents of a node.
contents :: Node -> [Content]
contents (Node str start offsets) = collect firstChild
  where
    collect i
      | i < endBoundary =
        case offsets ! i of
          0x00 ->
            Element
              (Node str i offsets) :
            collect (offsets ! (i + 4))
          0x01 ->
            Text (substring str (offsets ! (i + 1)) (offsets ! (i + 2))) :
            collect (i + 3)
          _ -> []
      | otherwise = []
    firstChild = go (start + 5)
      where
        go i | i < endBoundary =
          case offsets ! i of
            0x02 -> go (i + 5)
            _ -> i
          | otherwise = i
    endBoundary = offsets ! (start + 4)

-- | Attributes of a node.
attributes :: Node -> [(ByteString,ByteString)]
attributes (Node str start offsets) = collect (start + 5)
  where
    collect i
      | i < endBoundary =
        case offsets ! i of
          0x02 ->
            ( substring str (offsets ! (i + 1)) (offsets ! (i + 2))
            , substring str (offsets ! (i + 3)) (offsets ! (i + 4))) :
            collect (i + 5)
          _ -> []
      | otherwise = []
    endBoundary = offsets ! (start + 4)

-- | Name of the element.
name :: Node -> ByteString
name (Node str start offsets) =
  case offsets ! start of
    0x00 -> substring str (offsets ! (start + 2)) (offsets ! (start + 3))
    _ -> mempty

-- | Parse a complete Nodes document.
parse :: ByteString -> Node
parse str =
  (Node
     str
     0
     (runST
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
                    UMV.write v' (index + 2) name_start
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
                    UMV.write v' (index + 1) key_start
                    UMV.write v' (index + 2) key_len
                    UMV.write v' (index + 3) value_start
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
                    UMV.write v' (index + 1) text_start
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
              str
            wet <- readSTRef vecRef
            arr <- UV.unsafeFreeze wet
            size <- readRef sizeRef
            return (UV.unsafeSlice 0 size arr))))

-- | Get a substring of the BS.
substring :: ByteString -> Int -> Int -> ByteString
substring s' start len = S.take len (S.drop start s')
{-# INLINE substring #-}
