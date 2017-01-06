{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Test.Hspec
import           Text.XML.Hexml
import           Xeno.SAX

main = hspec spec

spec =
  describe
    "hexml tests"
    (mapM_
       (\(v, i) -> it (show i) (shouldBe (Xeno.SAX.validate i) v))
       hexml_examples_sax)

hexml_examples_sax :: [(Bool, ByteString)]
hexml_examples_sax =
    [(True, "<test id='bob'>here<extra/>there</test>")
    ,(True, "<test /><close />")
    ,(True, "<test /><!-- comment > --><close />")
    ,(True, "<test id=\"bob value\" another-attr=\"test with <\">here </test> more text at the end<close />")
    -- ,(False, "<test></more>")
    ,(False, "<test")
    ,(True, "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>")
    ]
