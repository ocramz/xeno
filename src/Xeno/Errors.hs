{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Simplifies raising and presenting localized exceptions to the user.
module Xeno.Errors(printExceptions
                  ,displayException
                  ,getStartIndex
                  ,failHere
                  ) where

import qualified Data.ByteString.Char8 as BS hiding (elem)
import           Data.ByteString.Internal(ByteString(..))
import           System.IO(stderr)

import           Xeno.Types

{-# NOINLINE failHere #-}
failHere :: BS.ByteString -> BS.ByteString -> Either XenoException a
failHere msg here = Left $ XenoParseError (getStartIndex here) msg

-- | Print schema errors with excerpts
printExceptions :: BS.ByteString -> [XenoException] -> IO ()
printExceptions i s = (BS.hPutStrLn stderr . displayException i) `mapM_` s

-- | Find line number of the error from ByteString index.
lineNo :: Int -> BS.ByteString -> Int
lineNo index bs = BS.count '\n'
                $ BS.take index bs

-- | Show for ByteStrings
bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

{-# INLINE CONLIKE getStartIndex #-}
getStartIndex :: BS.ByteString -> Int
getStartIndex (PS _ from _) = from

displayException :: BS.ByteString -> XenoException -> BS.ByteString
displayException input (XenoParseError i msg) =
               "Parse error in line " <> bshow (lineNo i input) <> ": "
            <> msg
            <> " at:\n"
            <> lineContentBeforeError
            <> lineContentAfterError
            <> "\n" <> pointer
  where
    lineContentBeforeError = snd $ BS.spanEnd   eoln $ revTake 40 $ BS.take i input
    lineContentAfterError  =       BS.takeWhile eoln $ BS.take 40 $ BS.drop i input
    pointer                = BS.replicate (BS.length lineContentBeforeError) ' ' <> "^"
    eoln ch                = ch /= '\n' && ch /= '\r'
displayException _      err                        = bshow err

-- | Take n last bytes.
revTake :: Int -> BS.ByteString -> BS.ByteString
revTake i (PS ptr from to) = PS ptr (end-len) len
  where
    end = from + to
    len = min to i

