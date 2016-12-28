-- | Benchmark memory allocations.

module Main where

import qualified Data.ByteString as S
import           Weigh
import qualified Xeno

main :: IO ()
main = do
  f4kb <- S.readFile "data/books-4kb.xml"
  f42kb <- S.readFile "data/ibm-oasis-42kb.xml"
  f52kb <- S.readFile "data/oasis-52kb.xml"
  mainWith
    (do func "4kb validate" Xeno.validate f4kb
        func "42kb validate" Xeno.validate f42kb
        func "52kb validate" Xeno.validate f52kb
        -- func "4kb parseErikd" Xeno.parseErikd f4kb
        -- func "42kb parseErikd" Xeno.parseErikd f42kb
        -- func "52kb parseErikd" Xeno.parseErikd f52kb
    )
