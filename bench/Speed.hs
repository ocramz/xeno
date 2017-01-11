{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Benchmark speed.

module Main where

import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           GHC.Generics
import qualified Text.XML.Expat.SAX as Hexpat
import qualified Text.XML.Expat.Tree as HexpatTree
import qualified Text.XML.Hexml as Hexml
import           Text.XML.Light as XML
import           Text.XML.Light.Input as XML
import qualified Xeno.SAX
import qualified Xeno.DOM

main :: IO ()
main =
  defaultMain
    [ env
        (S.readFile "data/books-4kb.xml")
        (\input ->
           bgroup
             "4KB"
             [ bench "hexml-dom" (whnf Hexml.parse input)
             , bench "xeno-sax" (whnf Xeno.SAX.validate input)
             , bench "xeno-dom" (whnf Xeno.DOM.parse input)
             , bench
                 "hexpat-sax"
                 (whnf
                    ((Hexpat.parseThrowing Hexpat.defaultParseOptions :: L.ByteString -> [Hexpat.SAXEvent ByteString ByteString]) .
                     L.fromStrict)
                    input)
             , bench
                 "hexpat-dom"
                 (whnf
                    ((HexpatTree.parse' HexpatTree.defaultParseOptions :: ByteString -> Either HexpatTree.XMLParseError (HexpatTree.Node ByteString ByteString)))
                    input)
             , bench "xml-dom" (nf XML.parseXMLDoc input)
             ])
    , env
        (S.readFile "data/text-31kb.xml")
        (\input ->
           bgroup
             "31KB"
             [ bench "hexml-dom" (whnf Hexml.parse input)
             , bench "xeno-sax" (whnf Xeno.SAX.validate input)
             , bench "xeno-dom" (whnf Xeno.DOM.parse input)
             , bench
                 "hexpat-sax"
                 (nf
                    ((Hexpat.parseThrowing Hexpat.defaultParseOptions :: L.ByteString -> [Hexpat.SAXEvent ByteString ByteString]) .
                     L.fromStrict)
                    input)
             , bench
                 "hexpat-dom"
                 (whnf
                    ((HexpatTree.parse' HexpatTree.defaultParseOptions :: ByteString -> Either HexpatTree.XMLParseError (HexpatTree.Node ByteString ByteString)))
                    input)
             , bench "xml-dom" (nf XML.parseXMLDoc input)
             ])
    , env
        (S.readFile "data/fabricated-211kb.xml")
        (\input ->
           bgroup
             "211KB"
             [ bench "hexml-dom" (whnf Hexml.parse input)
             , bench "xeno-sax" (whnf Xeno.SAX.validate input)
             , bench "xeno-dom" (whnf Xeno.DOM.parse input)
             , bench
                 "hexpat-sax"
                 (nf
                    ((Hexpat.parseThrowing Hexpat.defaultParseOptions :: L.ByteString -> [Hexpat.SAXEvent ByteString ByteString]) .
                     L.fromStrict)
                    input)
             , bench
                 "hexpat-dom"
                 (whnf
                    ((HexpatTree.parse' HexpatTree.defaultParseOptions :: ByteString -> Either HexpatTree.XMLParseError (HexpatTree.Node ByteString ByteString)))
                    input)
             , bench "xml-dom" (nf XML.parseXMLDoc input)
             ])
    ]

deriving instance Generic Content
deriving instance Generic Element
deriving instance Generic CData
deriving instance Generic CDataKind
deriving instance Generic QName
deriving instance Generic Attr
instance NFData Content
instance NFData Element
instance NFData CData
instance NFData CDataKind
instance NFData QName
instance NFData Attr
