{-# LANGUAGE OverloadedStrings #-}

-- | Simple test suite.

module Main where

import           Data.ByteString (ByteString)
import           Test.Hspec
import           Xeno.SAX
import           Xeno.DOM
import           Xeno.Types
-- import Control.Monad.Catch

import Data.Either

-- main = print "hello!"

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec =
  describe
    "hexml tests"
    (do mapM_
          (\(v, i) -> it (show i) (shouldBe (Xeno.SAX.validate i) v))
          hexml_examples_sax
        let doc =
              parse
                "<root><test id=\"1\" extra=\"2\" />\n<test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test /></root>"
        it
          "children test"
          (shouldBe
             (map name (children $ fromRightE doc))
             ["test", "test", "b", "test", "test"])
        it
          "attributes"
          (shouldBe
             (attributes (head (children $ fromRightE doc)))
             [("id", "1"), ("extra", "2")])
        -- If this works without crashing we're happy.
        let nsdoc = "<ns:tag os:attr=\"Namespaced attribute value\">Content.</ns:tag>"
        it
          "namespaces"
          (shouldBe
             (Xeno.SAX.validate nsdoc)
             True)
    )

hexml_examples_sax :: [(Bool, ByteString)]
hexml_examples_sax =
    [(True, "<test id='bob'>here<extra/>there</test>")
    ,(True, "<test /><close />")
    ,(True, "<test /><!-- comment > --><close />")
    ,(True, "<test id=\"bob value\" another-attr=\"test with <\">here </test> more text at the end<close />")
    ,(True, "<test></more>") -- SAX doesn't care about tag balancing
    ,(False, "<test")
    ,(True, "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>")
    ]


-- | Horrible hack. Don't try this at home.
fromRightE :: Either XenoException a -> a
fromRightE = either (error. show) id
