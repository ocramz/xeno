{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Efficient DOM data structure
module Xeno.DOM.Internal
  ( Node(..)
  , Content(..)
  , name
  , attributes
  , contents
  , children
  ) where

import           Control.DeepSeq
import           Data.ByteString          (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (ByteString(..))
import           Data.Data                (Data, Typeable)
import           Data.Mutable
import           Data.STRef
import           Data.Vector.Unboxed      ((!))
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

--import Debug.Trace
--trace _ a = a

-- | Some XML nodes.
data Node = Node !ByteString !Int !(UV.Vector Int)
  deriving (Eq, Data, Typeable)

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
  | CData {-# UNPACK #-}!ByteString
  deriving (Eq, Show, Data, Typeable)

instance NFData Content where
  rnf !_ = ()

-- | Get just element children of the node (no text).
children :: Node -> [Node]
children (Node str start offsets) = collect firstChild
  where
    collect i
      | i < endBoundary =
        case offsets ! i of
          0x00 -> Node str i offsets : collect (offsets ! (i + 4))
          0x01 -> collect (i + 3)
          _off -> [] -- trace ("Offsets " <> show i <> " is " <> show off) []
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
          0x03 ->
            CData (substring str (offsets ! (i + 1)) (offsets ! (i + 2))) :
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
    _ -> error "Node cannot have empty name" -- mempty

-- | Get a substring of the BS.
substring :: ByteString -> Int -> Int -> ByteString
substring s' start len = S.take len (S.drop start s')
{-# INLINE substring #-}
