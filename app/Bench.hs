{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Data.Time.Clock
import System.IO.Posix.MMap
import System.Mem
import Text.InterpolatedString.Perl6 (qc)
import Xeno.DOM
import Xeno.SAX
import qualified Data.ByteString as BS


main :: IO ()
main = do
    let prefix = "ex-data/"
        files' = map (prefix ++)
                [ {- 921 Mb -} "1htq.xml"
                , {- 190 Mb -} "enwiki-20190901-abstract10.xml"
                , {- 1.6 Gb -} "enwiki-20190901-pages-logging1.xml"
                , {- 4.0 Gb -} "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"
                -- , {-  21 Gb -} "enwiki-20190901-pages-meta-history2.xml"
                ]
        files = concat $ replicate 5 files'
        -- Benchmark with a lot of attributes:
        -- files = map (prefix ++) [ {- 1.6 Gb -} "enwiki-20190901-pages-logging1.xml" ]
    --
    deltas <- forM files $ \fn -> do
        putStrLn [qc|Processing file '{fn}'|]
        --
        -- NOTE: It is need to cache file in memory BEFORE start test.
        --       It can be done with `vmtouch` utility for example (`vmtouch -vtL *`).
        --
        bs <- unsafeMMapFile fn
        -- bs <- BS.readFile fn
        putStrLn [qc|  size: {BS.length bs `div` (1024*1024)} Mb|]
        performGC
        start <- getCurrentTime
        -- SAX:
        -- let res = validate bs
        -- putStrLn [qc|  process result: {res}|]
        -- DOM:
        (\(Right !_node) -> putStrLn [qc|  processed!|]) (parse bs)
        finish <- getCurrentTime
        let delta = finish `diffUTCTime` start
        putStrLn [qc|  processing time: {delta}|]
        return delta
    --
    putStrLn "------"
    putStrLn [qc|Total: {sum deltas}|]
