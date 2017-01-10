{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Using the SAX parser, provide a simple tree interface.

module Xeno.Tree where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Monoid
import GHC.Generics
import Xeno.SAX

data Node
  = Element !ByteString ![(ByteString, ByteString)] ![Node]
  | Text !ByteString
  deriving (Show, Generic, NFData)

data State
  = Start
  | Nodes [Node]
  | Open ByteString [(ByteString,ByteString)] [Node] State
  deriving (Show, Generic, NFData)

parse :: ByteString -> [Node]
parse i =
  case run i of
    Start -> []
    Nodes ns -> ns
    Open {} -> error "Missing closing tag."
  where
    run =
      fold
        (\s name -> Open name [] [] s)
        (\s key value ->
           case s of
             Open name attrs cs s' -> Open name ((key, value) : attrs) cs s'
             _ -> error "Unexpected attributes.")
        (\s _ -> s)
        (\s text ->
           let node = Text text
           in case s of
                Start -> Nodes [node]
                Nodes ns -> Nodes (ns ++ [node])
                Open name attrs cs s' -> Open name attrs (cs ++ [node]) s')
        (\s name0 ->
           case s of
             Open name attrs cs s'
               | name == name0 ->
                 let node = Element name attrs cs
                 in case s' of
                      Start -> Nodes [node]
                      Nodes cs' -> Nodes (cs' ++ [node])
                      Open name' attrs' cs' s'' ->
                        Open name' attrs' (cs' ++ [node]) s''
             _ -> error "Unexpected closing attribute.")
        Start

render :: [Node] -> ByteString
render = mconcat . map go
  where
    go (Text t) = t
    go (Element name attrs children) =
      "<" <> name <>
      mconcat (map (\(key, val) -> " " <> key <> "=\"" <> val <> "\"") attrs) <>
      ">" <>
      render children <>
      "</" <>
      name <>
      ">"
