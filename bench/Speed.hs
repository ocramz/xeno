{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Benchmark speed.

module Main where

import           Criterion
import           Criterion.Main
import qualified Data.ByteString as S
import qualified Text.XML.Hexml as Hexml
import qualified Xeno
import qualified Xeno.Vectorize

main :: IO ()
main =
  defaultMain
    [ env
        (S.readFile "data/books-4kb.xml")
        (\input ->
           bgroup
             "4KB"
             [ bench "hexml" (whnf Hexml.parse input)
             , bench "xeno" (whnf Xeno.validate input)
             , bench "xeno-vectorize" (whnf Xeno.Vectorize.parse input)
             ])
    , env
        (S.readFile "data/text-31kb.xml")
        (\input ->
           bgroup
             "31KB"
             [ bench "hexml" (whnf Hexml.parse input)
             , bench "xeno" (whnf Xeno.validate input)
             , bench "xeno-vectorize" (whnf Xeno.Vectorize.parse input)
             ])
    , env
        (S.readFile "data/fabricated-211kb.xml")
        (\input ->
           bgroup
             "211KB"
             [ bench "hexml" (whnf Hexml.parse input)
             , bench "xeno" (whnf Xeno.validate input)
             , bench "xeno-vectorize" (whnf Xeno.Vectorize.parse input)
             ])
    ]
