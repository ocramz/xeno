{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Benchmark speed.

module Main where

import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import qualified Data.ByteString as S
import           GHC.Generics
import qualified Text.XML.Hexml as Hexml
import           Text.XML.Light
import           Text.XML.Light as XML
import qualified Xeno

main :: IO ()
main =
  defaultMain
    [ env
        (S.readFile "data/books-4kb.xml")
        (\input ->
           bgroup
             "4KB"
             [ bench "hexml" (whnf Hexml.parse input)
             , bench "xeno" (whnf Xeno.parse input)
             , bench "xml" (nf XML.parseXMLDoc input)
             ])
    , env
        (S.readFile "data/ibm-oasis-42kb.xml")
        (\input ->
           bgroup
             "42KB"
             [ bench "hexml" (whnf Hexml.parse input)
             , bench "xeno" (whnf Xeno.parse input)
             , bench "xml" (nf XML.parseXMLDoc input)
             ])
    , env
        (S.readFile "data/oasis-52kb.xml")
        (\input ->
           bgroup
             "52KB"
             [ bench "hexml" (whnf Hexml.parse input)
             , bench "xeno" (whnf Xeno.parse input)
             , bench "xml" (nf XML.parseXMLDoc input)
             ])
    , env
        (S.readFile "data/japanese-182kb.xml")
        (\input ->
           bgroup
             "182KB"
             [ bench "hexml" (whnf Hexml.parse input)
             , bench "xeno" (whnf Xeno.parse input)
             , bench "xml" (nf XML.parseXMLDoc input)
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
