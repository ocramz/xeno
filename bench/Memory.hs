{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

-- | Benchmark memory allocations.

module Main where

import           Control.DeepSeq
import qualified Data.ByteString as S
import qualified Text.XML.Hexml as Hexml
import           Weigh
import qualified Xeno.DOM
import qualified Xeno.RobustDOM
import qualified Xeno.SAX

main :: IO ()
main = do
  f4kb <- S.readFile "data/books-4kb.xml"
  f31kb <- S.readFile "data/text-31kb.xml"
  f211kb <- S.readFile "data/fabricated-211kb.xml"
  mainWith
    (do func "4kb/hexml/dom" Hexml.parse f4kb
        func "4kb/xeno/sax" Xeno.SAX.validate f4kb
        func "4kb/xeno/dom" Xeno.DOM.parse f4kb
        func "4kb/xeno/dom-with-recovery" Xeno.RobustDOM.parse f4kb
        func "31kb/hexml/dom" Hexml.parse f31kb
        func "31kb/xeno/sax" Xeno.SAX.validate f31kb
        func "31kb/xeno/dom" Xeno.DOM.parse f31kb
        func "31kb/xeno/dom-with-recovery" Xeno.RobustDOM.parse f31kb
        func "211kb/hexml/dom" Hexml.parse f211kb
        func "211kb/xeno/sax" Xeno.SAX.validate f211kb
        func "211kb/xeno/dom" Xeno.DOM.parse f211kb
        func "211kb/xeno/dom-with-recovery" Xeno.RobustDOM.parse f211kb)

instance NFData Hexml.Node where
  rnf !_ = ()
