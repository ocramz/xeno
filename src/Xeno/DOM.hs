{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Using the SAX parser, provide a DOM interface.

module Xeno.DOM
  ( parse
  , Content
  , Node
  ) where

import           Control.DeepSeq
import           Control.Monad.ST
import           Data.ByteString (ByteString)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString.Internal (ByteString(PS))
import           Data.IntRef
import           Data.STRef
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Xeno.SAX

-- | Some XML nodes.
newtype Content = Content (ByteString, UV.Vector Int)
  deriving (NFData, Eq)

instance Show Content where
  show (Content (bs,_)) = S8.unpack bs

newtype Node = Node (UV.Vector Int)
  deriving (NFData, Eq)

-- | Parse a complete Nodes document.
parse :: ByteString -> Content
parse str =
  Content
    (str
    ,runST
       (do nil <- UMV.new 1000
           vecRef <- newSTRef nil
           sizeRef <- newIntRef 0
           parentRef <- newIntRef 0
           process
             (\(PS _ name_start name_len) -> do
                let tag = 0x00
                    tag_end = -1
                index <- readIntRef sizeRef
                v' <-
                  do v <- readSTRef vecRef
                     if index + 5 < UMV.length v
                       then pure v
                       else do
                         v' <- UMV.grow v (UMV.length v)
                         writeSTRef vecRef v'
                         return v'
                do writeIntRef parentRef index
                   writeIntRef sizeRef (index + 5)
                do UMV.write v' index tag
                   tag_parent <- readIntRef parentRef
                   UMV.write v' (index + 1) tag_parent
                   UMV.write v' (index + 2) name_start
                   UMV.write v' (index + 3) name_len
                   UMV.write v' (index + 4) tag_end)
             (\(PS _ key_start key_len) (PS _ value_start value_len) -> do
                index <- readIntRef sizeRef
                v' <-
                  do v <- readSTRef vecRef
                     if index + 5 < UMV.length v
                       then pure v
                       else do
                         v' <- UMV.grow v (UMV.length v)
                         writeSTRef vecRef v'
                         return v'
                let tag = 0x02
                do writeIntRef sizeRef (index + 5)
                do UMV.write v' index tag
                   UMV.write v' (index + 1) key_start
                   UMV.write v' (index + 2) key_len
                   UMV.write v' (index + 3) value_start
                   UMV.write v' (index + 4) value_len)
             (\_ -> return ())
             (\(PS _ text_start text_len) -> do
                let tag = 0x01
                index <- readIntRef sizeRef
                v' <-
                  do v <- readSTRef vecRef
                     if index + 3 < UMV.length v
                       then pure v
                       else do
                         v' <- UMV.grow v (UMV.length v)
                         writeSTRef vecRef v'
                         return v'
                do writeIntRef sizeRef (index + 3)
                do UMV.write v' index tag
                   UMV.write v' (index + 1) text_start
                   UMV.write v' (index + 2) text_len)
             (\_ -> do
                index <- readIntRef sizeRef
                v <- readSTRef vecRef
                parent <- readIntRef parentRef
                UMV.write v (parent + 4) index
                previousParent <- UMV.read v (parent + 1)
                writeIntRef parentRef previousParent)
             str
           wet <- readSTRef vecRef
           arr <- UV.unsafeFreeze wet
           size <- readIntRef sizeRef
           return (UV.unsafeSlice 0 size arr)))

chuck :: ByteString -> Content -> IO ()
chuck original (Content (s,buffer)) = go 0
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
                               (\j k -> "[" ++ show j ++ "] " ++ k)
                               [i ..]
                               [ "type = tag"
                               , "tag_parent = " ++ show parent
                               , "name_start = " ++ show name_start
                               , "name_end = " ++ show name_end
                               , "tag_end = " ++ show tag_end
                               , "name: " ++
                                 show (substring original name_start name_end)
                               ]))
                       go (i + 5)
               _ ->
                 let text_end = buffer ! (i + 2)
                     text_start = buffer ! (i + 1)
                 in do putStrLn
                         (unlines
                            (zipWith
                               (\j k -> "[" ++ show j ++ "] " ++ k)
                               [i ..]
                               [ "type = text"
                               , "text_start = " ++ show text_start
                               , "text_end = " ++ show text_end
                               , "text: " ++
                                 show (substring original text_start text_end)
                               ]))
                       go (i + 3)
        else return ()
    substring :: ByteString -> Int -> Int -> ByteString
    substring s start len = S.take len (S.drop start s)
