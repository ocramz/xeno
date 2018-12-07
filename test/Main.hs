{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple test suite.

module Main where

import           Data.Either (isRight)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Test.Hspec
--import qualified Test.Hspec as Hspec(it)
import           Xeno.SAX  (validate)
import           Xeno.DOM  (Content(..), parse, name, contents, attributes, children)
import qualified Xeno.RobustDOM as RDOM
import           Xeno.Types
import           System.Timeout
import qualified Debug.Trace as Debug(trace)

main :: IO ()
main = hspec spec

{-
withinTime  :: (HasCallStack) => Int -> IO Expectation -> Expectation
withinTime t action = do
  r <- timeout  t action
  case r of
    Just !r  -> r
    Nothing -> expectationFailure $ "did not finish within allocated time"
 -}

{-
timedAction action = do
    timeout (return $! action)
  where
    check a = 
 -}

{-
it :: Example a => String -> a -> SpecWith (Arg a)
it s = Hspec.it s . withinTime (5*second) . pure
  where
    second = 1000000-}

spec :: SpecWith ()
spec = do
  describe "Xeno.DOM tests" $ do
    it "DOM from bytestring substring" $ do
      let substr = BS.drop 5 "5<8& <valid>xml<here/></valid>"
          parsedRoot = fromRightE $ parse substr
      name parsedRoot `shouldBe` "valid"

    it "Leading whitespace characters are accepted by parse" $ 
      isRight (parse "\n<a></a>") `shouldBe` True

    let doc =
              parse
                "<root><test id=\"1\" extra=\"2\" />\n<test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test /></root>"

    it "children test" $
      map name (children $ fromRightE doc) `shouldBe` ["test", "test", "b", "test", "test"]
      
    it "attributes" $ 
      attributes (head (children $ fromRightE doc)) `shouldBe` [("id", "1"), ("extra", "2")]

    it "xml prologue test" $ do
      let docWithPrologue = "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>"
          parsedRoot = fromRightE $ Xeno.DOM.parse docWithPrologue
      name parsedRoot `shouldBe` "greeting"                
    it "html doctype test" $ do
      let docWithPrologue = "<!DOCTYPE html>\n<greeting>Hello, world!</greeting>"
          parsedRoot = fromRightE $ Xeno.DOM.parse docWithPrologue
      name parsedRoot `shouldBe` "greeting"
      
  describe
    "hexml tests"
    (do mapM_
          (\(v, i) -> it (show i) (shouldBe (validate i) v))
          (hexml_examples_sax  ++ extra_examples_sax)
        mapM_
          (\(v, i) -> it (show i) (shouldBe (either (Left . show) (Right . id) (contents <$> parse i)) v))
          cdata_tests
 
       -- If this works without crashing we're happy.
        let nsdoc = "<ns:tag os:attr=\"Namespaced attribute value\">Content.</ns:tag>"
        it
          "namespaces" $
          validate nsdoc `shouldBe` True
    )
  describe "robust XML tests" $ do
    it "DOM from bytestring substring" $ do
        let substr = BS.drop 5 "5<8& <valid>xml<here/></valid>"
            parsedRoot = fromRightE $ RDOM.parse substr
        name parsedRoot `shouldBe` "valid"

    it "Leading whitespace characters are accepted by parse" $ 
      isRight (RDOM.parse "\n<a></a>") `shouldBe` True

    let doc =
              RDOM.parse
                "<root><test id=\"1\" extra=\"2\" />\n<test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test /></root>"

    it "children test" $
      map name (children $ fromRightE doc) `shouldBe` ["test", "test", "b", "test", "test"]
      
    it "attributes" $ 
      attributes (head (children $ fromRightE doc)) `shouldBe` [("id", "1"), ("extra", "2")]

    it "xml prologue test" $ do
      let docWithPrologue = "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>"
          parsedRoot = fromRightE $ RDOM.parse docWithPrologue
      name parsedRoot `shouldBe` "greeting"                
    it "html doctype test" $ do
      let docWithPrologue = "<!DOCTYPE html>\n<greeting>Hello, world!</greeting>"
          parsedRoot = fromRightE $ RDOM.parse docWithPrologue
      name parsedRoot `shouldBe` "greeting"
       
    describe
      "hexml tests"
      (do mapM_
            (\(v, i) -> it (show i) (shouldBe (validate i) v))
            (hexml_examples_sax  ++ extra_examples_sax)
          mapM_
            (\(v, i) -> it (show i) (shouldBe (either (Left . show) (Right . id) (contents <$> parse i)) v))
            cdata_tests
   
         -- If this works without crashing we're happy.
          let nsdoc = "<ns:tag os:attr=\"Namespaced attribute value\">Content.</ns:tag>"
          it
            "namespaces" $
            validate nsdoc `shouldBe` True
      )
    it "recovers unclosed tag" $ do
      let parsed = RDOM.parse "<a attr='a'><img></a>"
      Debug.trace (show parsed) $ do
        name (fromRightE parsed) `shouldBe` "a"
        RDOM.attributes (fromRightE parsed) `shouldBe` [("attr", "a")]
        map name (RDOM.children $ fromRightE parsed) `shouldBe` ["img"]
    it "ignores too many closing tags" $ do
      let parsed = RDOM.parse "<a></a></b></c>"
      isRight parsed `shouldBe` True

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

extra_examples_sax :: [(Bool, ByteString)]
extra_examples_sax =
    [(True, "<some-example/>")
    ,(True, "<a numeric1=\"attribute\"/>")
    ,(True, "<also.a.dot></also.a.dot>")
    ]

-- | We want to make sure that the parser doesn't jump out of the CDATA
-- area prematurely because it encounters a single ].
cdata_tests :: [(Either a [Content], ByteString)]
cdata_tests =
    [ ( Right [CData "Oneliner CDATA."]
      , "<test><![CDATA[Oneliner CDATA.]]></test>")
    , ( Right [CData "<strong>This is strong but not XML tags.</strong>"]
      , "<test><![CDATA[<strong>This is strong but not XML tags.</strong>]]></test>")
    , ( Right [CData "A lonely ], sad isn't it?"]
      , "<test><![CDATA[A lonely ], sad isn't it?]]></test>")
    ]

-- | Horrible hack. Don't try this at home.
fromRightE :: Either XenoException a -> a
fromRightE = either (error . show) id


mapLeft :: Applicative f => (a -> f b) -> Either a b -> f b
mapLeft f = either f pure

mapRight :: Applicative f => (b -> f a) -> Either a b -> f a
mapRight = either pure
