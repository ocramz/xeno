{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-unused-imports #-}

-- | Benchmark speed with big files

module Main where


import           Codec.Compression.BZip
import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.List (delete)
import           GHC.Generics
import           System.FilePath.Posix
import qualified Text.XML.Expat.SAX as Hexpat
import qualified Text.XML.Expat.Tree as HexpatTree
import qualified Text.XML.Hexml as Hexml
import           Text.XML.Light as XML
import           Text.XML.Light.Input as XML
import qualified Xeno.Types
import qualified Xeno.SAX
import qualified Xeno.DOM
import qualified Xeno.DOM.Robust
import qualified Data.ByteString as S
#ifdef LIBXML2
import qualified Text.XML.LibXML.Parser as Libxml2
#endif


main :: IO ()
main = defaultMain
    [ benchFile allTests       "46MB"  "enwiki-20190901-pages-articles14.xml-p7697599p7744799.bz2"
    , benchFile allTests       "624MB" "enwiki-20190901-pages-articles-multistream1.xml-p10p30302.bz2"
    , benchFile allTests       "921MB" "1HTQ.xml.bz2"
    , benchFile allTests       "1.6Gb" "enwiki-20190901-pages-meta-current6.xml-p565314p892912.bz2"
    , benchFile allExceptHexml "4Gb"   "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.bz2"
    -- , benchFile allExceptHexml "21Gb"  "enwiki-20190901-pages-meta-history2.xml-p31255p31720.bz2"
    ]


allTests :: [String]
allTests = [ "hexml-dom"
           , "xeno-sax"
           , "xeno-sax-z"
           -- , "xeno-sax-ex"
           -- , "xeno-sax-ex-z"
           , "xeno-dom"
           , "xeno-dom-with-recovery"
           -- XXX: "hexpact", "xml-dom" library don't work with big files; require too much memory
           -- , "hexpat-sax"
           -- , "hexpat-dom"
           -- , "xml-dom"
           -- , "libxml2-dom"
           ]


allExceptHexml :: [String]
allExceptHexml = "hexml-dom" `delete` allTests


benchFile :: [String] -> String -> FilePath -> Benchmark
benchFile enabledTests size fn =
    env (readBZip2File fn)
        (\ ~(input, inputz) -> bgroup size $ benchMethods enabledTests input inputz)


benchMethods :: [String] -> ByteString -> Xeno.Types.ByteStringZeroTerminated -> [Benchmark]
benchMethods enabledTests input inputz =
       runBench "hexml-dom" (whnf Hexml.parse input)
    ++ runBench "xeno-sax"      (whnf Xeno.SAX.validate input)
    ++ runBench "xeno-sax-z"    (whnf Xeno.SAX.validate inputz)
    ++ runBench "xeno-sax-ex  " (whnf Xeno.SAX.validateEx input)
    ++ runBench "xeno-sax-ex-z" (whnf Xeno.SAX.validateEx inputz)
    ++ runBench "xeno-dom" (whnf Xeno.DOM.parse input)
    ++ runBench "xeno-dom-with-recovery" (whnf Xeno.DOM.Robust.parse input)
    ++ runBench
        "hexpat-sax"
        (whnf
            ((Hexpat.parseThrowing Hexpat.defaultParseOptions :: L.ByteString -> [Hexpat.SAXEvent ByteString ByteString]) .
             L.fromStrict)
            input)
    ++ runBench
        "hexpat-dom"
        (whnf
            ((HexpatTree.parse' HexpatTree.defaultParseOptions :: ByteString -> Either HexpatTree.XMLParseError (HexpatTree.Node ByteString ByteString)))
            input)
    ++ runBench "xml-dom" (nf XML.parseXMLDoc input)
#ifdef LIBXML2
    ++ runBench "libxml2-dom" (whnfIO (Libxml2.parseMemory input))
#endif
  where
    runBench name act
        | name `elem` enabledTests = [bench name act]
        | otherwise                = []


readBZip2File :: FilePath -> IO (ByteString, Xeno.Types.ByteStringZeroTerminated)
readBZip2File fn = do
    file <- L.readFile ("data" </> "ex" </> fn)
    let !bs  = L.toStrict $ decompress file
        !bsz = Xeno.Types.BSZT $ bs `S.snoc` 0
    return (bs, bsz)


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
