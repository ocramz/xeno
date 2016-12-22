-- | Benchmark memory allocations.

module Main where

import qualified Data.ByteString as S
import           Weigh
import qualified Xeno

main :: IO ()
main = do
  mainWith
    (do io
          "4kb parse"
          (\fp -> fmap Xeno.parse (S.readFile fp))
          "data/books-4kb.xml"
        io
          "42kb parse"
          (\fp -> fmap Xeno.parse (S.readFile fp))
          "data/ibm-oasis-42kb.xml"
        io
          "52kb parse"
          (\fp -> fmap Xeno.parse (S.readFile fp))
          "data/oasis-52kb.xml"
        io
          "182kb parse"
          (\fp -> fmap Xeno.parse (S.readFile fp))
          "data/japanese-182kb.xml")
