{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple test suite.

module Main where

import           Data.Either (isRight)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Test.Hspec
import           Xeno.SAX  (validate, skipDoctype)
import           Xeno.DOM  (Node, Content(..), parse, name, contents, attributes, children)
import qualified Xeno.DOM.Robust as RDOM
import           Xeno.Types
import qualified Debug.Trace as Debug(trace)

main :: IO ()
main = hspec spec


spec :: SpecWith ()
spec = do
  describe "Xeno.DOM tests" $ do
    it "test 1" $ do
      xml <- BS.readFile "data/books-4kb.xml"
      let (Right dom) = parse xml
      (name dom) `shouldBe` "catalog"
      (length $ contents dom) `shouldBe` 25
      (length $ children dom) `shouldBe` 12
      (length $ allChildrens dom) `shouldBe` 84
      (length $ concatMap attributes $ allChildrens dom) `shouldBe` 12
      (concatMap attributes $ allChildrens dom) `shouldBe`
          [("id","bk101"),("id","bk102"),("id","bk103"),("id","bk104")
          ,("id","bk105"),("id","bk106"),("id","bk107"),("id","bk108")
          ,("id","bk109"),("id","bk110"),("id","bk111"),("id","bk112")]
      (map name $ allChildrens dom) `shouldBe`
          (replicate 12 "book" ++ (concat $
          replicate 12 ["author","title","genre","price","publish_date","description"]))
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

  describe
    "hexml tests"
    (do mapM_
          (\(v, i) -> it (show i) (shouldBe (validate i) v))
          (hexml_examples_sax  ++ extra_examples_sax)
        mapM_
          (\(v, i) -> it (show i) (shouldBe (either (Left . show) (Right . id) (contents <$> parse i)) v))
          cdata_tests

       -- If this works without crashing we're happy.
        let nsdoc = ("<ns:tag os:attr=\"Namespaced attribute value\">Content.</ns:tag>" :: ByteString)
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
          let nsdoc = ("<ns:tag os:attr=\"Namespaced attribute value\">Content.</ns:tag>" :: ByteString)
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
  describe "skipDoctype" $ do
    it "strips initial doctype declaration" $ do
      skipDoctype "<!DOCTYPE html><?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello" `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello"
    it "strips doctype after spaces" $ do
      skipDoctype "  \n<!DOCTYPE html><?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello" `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello"
    it "does not strip anything after or inside element" $ do
      let insideElt = "<xml><?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello</xml>"
      skipDoctype  insideElt `shouldBe` insideElt

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

allChildrens :: Node -> [Node]
allChildrens n = allChildrens' [n]
  where
    allChildrens' :: [Node] -> [Node]
    allChildrens' [] = []
    allChildrens' ns =
        let nextNodes = concatMap children ns
        in nextNodes ++ (allChildrens' nextNodes)

