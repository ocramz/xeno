-- | Benchmark memory allocations.

module Main where

import qualified Data.ByteString as S
import           Weigh
import qualified Xeno
import qualified Xeno.Vectorize

main :: IO ()
main = do
  f4kb <- S.readFile "data/books-4kb.xml"
  f31kb <- S.readFile "data/text-31kb.xml"
  f211kb <- S.readFile "data/fabricated-211kb.xml"
  mainWith
    (do func "4kb validate" Xeno.validate f4kb
        func "31kb validate" Xeno.validate f31kb
        func "211kb validate" Xeno.validate f211kb
        func "4kb " Xeno.Vectorize.parse f4kb
        func "31kb" Xeno.Vectorize.parse f31kb
        func "211kb" Xeno.Vectorize.parse f211kb)
