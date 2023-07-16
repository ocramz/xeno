{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | SAX parser and API for XML.
module Xeno.SAX
  ( process
  , Process(..)
  , StringLike(..)
  , fold
  , validate
  , validateEx
  , dump
  , skipDoctype
  , skipXml
  ) where

import           Control.Exception (throw)
import           Control.Monad (unless)
import           Control.Monad.ST (ST, runST)
import           Control.Monad.State.Strict (State, evalStateT, execState, modify', lift, get, put)
import           Control.Spork (spork)
import           Data.Bits (testBit)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Unsafe as SU
import           Data.Char (isSpace)
import           Data.Functor.Identity (Identity(..))
import           Data.Semigroup ()
import           Data.STRef (newSTRef, modifySTRef', readSTRef)
import           Data.Word (Word8, Word64)
import           Xeno.Types


class StringLike str where
    s_index'       :: str -> Int -> Word8
    elemIndexFrom' :: Word8 -> str -> Int -> Maybe Int
    drop'          :: Int -> str -> str
    substring'     :: str -> Int -> Int -> ByteString
    toBS           :: str -> ByteString

instance StringLike ByteString where
    s_index'       = s_index
    {-# INLINE s_index' #-}
    elemIndexFrom' = elemIndexFrom
    {-# INLINE elemIndexFrom' #-}
    drop'          = S.drop
    {-# INLINE drop' #-}
    substring'     = substring
    {-# INLINE substring' #-}
    toBS           = id
    {-# INLINE toBS #-}

instance StringLike ByteStringZeroTerminated where
    s_index' (BSZT ps) n = ps `SU.unsafeIndex` n
    {-# INLINE s_index' #-}
    elemIndexFrom' w (BSZT bs) i = elemIndexFrom w bs i
    {-# INLINE elemIndexFrom' #-}
    drop' i (BSZT bs) = BSZT $ S.drop i bs
    {-# INLINE drop' #-}
    substring' (BSZT bs) s t = substring bs s t
    {-# INLINE substring' #-}
    toBS (BSZT bs) = bs
    {-# INLINE toBS #-}

-- | Parameters to the 'process' function
data Process a =
  Process {
      openF    :: !(ByteString ->               a) -- ^ Open tag.
    , attrF    :: !(ByteString -> ByteString -> a) -- ^ Tag attribute.
    , endOpenF :: !(ByteString ->               a) -- ^ End open tag.
    , textF    :: !(ByteString ->               a) -- ^ Text.
    , closeF   :: !(ByteString ->               a) -- ^ Close tag.
    , cdataF   :: !(ByteString ->               a) -- ^ CDATA.
    }

--------------------------------------------------------------------------------
-- Helpful interfaces to the parser

-- | Parse the XML but return no result, process no events.
--
-- N.B.: Only the lexical correctness of the input string is checked, not its XML semantics (e.g. only if tags are well formed, not whether tags are properly closed)
--
-- > > :set -XOverloadedStrings
-- > > validate "<b>"
-- > True
--
-- > > validate "<b"
-- > False
validate :: (StringLike str) => str -> Bool
validate s =
  case spork
         (runIdentity
            (process
               Process {
                 openF    = \_   -> pure ()
               , attrF    = \_ _ -> pure ()
               , endOpenF = \_   -> pure ()
               , textF    = \_   -> pure ()
               , closeF   = \_   -> pure ()
               , cdataF   = \_   -> pure ()
               }
               s)) of
    Left (_ :: XenoException) -> False
    Right _ -> True
-- It must be inlined or specialised to ByteString/ByteStringZeroTerminated
{-# INLINE validate #-}
{-# SPECIALISE validate :: ByteString -> Bool #-}
{-# SPECIALISE validate :: ByteStringZeroTerminated -> Bool #-}


-- | Parse the XML and checks tags nesting.
--
validateEx :: (StringLike str) => str -> Bool
validateEx s =
  case spork
         (runST $ do
            tags <- newSTRef []
            (process
               Process {
                 openF    = \tag   -> modifySTRef' tags (tag:)
               , attrF    = \_ _ -> pure ()
               , endOpenF = \_   -> pure ()
               , textF    = \_   -> pure ()
               , closeF   = \tag  ->
                   modifySTRef' tags $ \case
                      [] -> fail $ "Unexpected close tag \"" ++ show tag ++ "\""
                      (expectedTag:tags') ->
                          if expectedTag == tag
                          then tags'
                          else fail $ "Unexpected close tag. Expected \"" ++ show expectedTag ++ "\", but got \"" ++ show tag ++ "\""
               , cdataF   = \_   -> pure ()
               }
               s)
            readSTRef tags >>= \case
                [] -> return ()
                tags' -> error $ "Not all tags closed: " ++ show tags'
         ) of
    Left (_ :: XenoException) -> False
    Right _ -> True
{-# INLINE validateEx #-}
{-# SPECIALISE validateEx :: ByteString -> Bool #-}
{-# SPECIALISE validateEx :: ByteStringZeroTerminated -> Bool #-}


-- | Parse the XML and pretty print it to stdout.
dump :: ByteString -> IO ()
dump str =
  evalStateT
    (process
       Process {
         openF = \name -> do
          level <- get
          lift (S8.putStr (S8.replicate level ' ' <> "<" <> name <> ""))
       , attrF = \key value -> lift (S8.putStr (" " <> key <> "=\"" <> value <> "\""))
       , endOpenF = \_ -> do
          level <- get
          let !level' = level + 2
          put level'
          lift (S8.putStrLn (">"))
       , textF = \text -> do
          level <- get
          lift (S8.putStrLn (S8.replicate level ' ' <> S8.pack (show text)))
       , closeF = \name -> do
          level <- get
          let !level' = level - 2
          put level'
          lift (S8.putStrLn (S8.replicate level' ' ' <> "</" <> name <> ">"))
       , cdataF = \cdata -> do
          level <- get
          lift (S8.putStrLn (S8.replicate level ' ' <> "CDATA: " <> S8.pack (show cdata)))
       }
       str)
    (0 :: Int)

-- | Fold over the XML input.
fold
  :: (s -> ByteString -> s) -- ^ Open tag.
  -> (s -> ByteString -> ByteString -> s) -- ^ Attribute key/value.
  -> (s -> ByteString -> s) -- ^ End of open tag.
  -> (s -> ByteString -> s) -- ^ Text.
  -> (s -> ByteString -> s) -- ^ Close tag.
  -> (s -> ByteString -> s) -- ^ CDATA.
  -> s
  -> ByteString
  -> Either XenoException s
fold openF attrF endOpenF textF closeF cdataF s str =
  spork
    (execState
       (process Process {
            openF    = \name -> modify' (\s' -> openF s' name)
          , attrF    = \key value -> modify' (\s' -> attrF s' key value)
          , endOpenF = \name -> modify' (\s' -> endOpenF s' name)
          , textF    = \text -> modify' (\s' -> textF s' text)
          , closeF   = \name -> modify' (\s' -> closeF s' name)
          , cdataF   = \cdata -> modify' (\s' -> cdataF s' cdata)
        } str)
       s)

--------------------------------------------------------------------------------
-- Main parsing function

-- | Process events with callbacks in the XML input.
process
  :: (Monad m, StringLike str)
  => Process (m ())
  -> str
  -> m ()
process !(Process {openF, attrF, endOpenF, textF, closeF, cdataF}) str = findLT 0
  where
    findLT index =
      case elemIndexFrom' openTagChar str index of
        Nothing -> unless (S.null text) (textF text)
          where text = toBS $ drop' index str
        Just fromLt -> do
          unless (S.null text) (textF text)
          checkOpenComment (fromLt + 1)
          where text = substring' str index fromLt
    -- Find open comment, CDATA or tag name.
    checkOpenComment index
      | s_index' this 0 == bangChar -- !
      , s_index' this 1 == commentChar -- -
      , s_index' this 2 == commentChar -- -
      =  findCommentEnd (index + 3)

      | s_index' this 0 == bangChar -- !
      , s_index' this 1 == openAngleBracketChar -- [
      , s_index' this 2 == 67 -- C
      , s_index' this 3 == 68 -- D
      , s_index' this 4 == 65 -- A
      , s_index' this 5 == 84 -- T
      , s_index' this 6 == 65 -- A
      , s_index' this 7 == openAngleBracketChar -- [
      =  findCDataEnd (index + 8) (index + 8)

      | otherwise
      = findTagName index
      where
        this = drop' index str
    findCommentEnd index =
      case elemIndexFrom' commentChar str index of
        Nothing -> throw $ XenoParseError index "Couldn't find the closing comment dash."
        Just fromDash ->
          if s_index' this 0 == commentChar && s_index' this 1 == closeTagChar
            then findLT (fromDash + 2)
            else findCommentEnd (fromDash + 1)
          where this = drop' index str
    findCDataEnd cdata_start index =
      case elemIndexFrom' closeAngleBracketChar str index of
        Nothing -> throw $ XenoParseError index "Couldn't find closing angle bracket for CDATA."
        Just fromCloseAngleBracket ->
          if s_index' str (fromCloseAngleBracket + 1) == closeAngleBracketChar
             then do
               cdataF (substring' str cdata_start fromCloseAngleBracket)
               findLT (fromCloseAngleBracket + 3) -- Start after ]]>
             else
               -- We only found one ], that means that we need to keep searching.
               findCDataEnd cdata_start (fromCloseAngleBracket + 1)
    findTagName index0
      | s_index' str index0 == questionChar =
        case elemIndexFrom' closeTagChar str spaceOrCloseTag of
          Nothing -> throw $ XenoParseError index "Couldn't find the end of the tag."
          Just fromGt -> do
            findLT (fromGt + 1)
      | s_index' str spaceOrCloseTag == closeTagChar = do
        let tagname = substring' str index spaceOrCloseTag
        if s_index' str index0 == slashChar
          then closeF tagname
          else do
            openF tagname
            endOpenF tagname
        findLT (spaceOrCloseTag + 1)
      | otherwise = do
        let tagname = substring' str index spaceOrCloseTag
        openF tagname
        result <- findAttributes spaceOrCloseTag
        endOpenF tagname
        case result of
          Right closingTag -> findLT (closingTag + 1)
          Left closingPair -> do
            closeF tagname
            findLT (closingPair + 2)
      where
        index =
          if s_index' str index0 == slashChar
            then index0 + 1
            else index0
        spaceOrCloseTag = parseName str index
    findAttributes index0
      -- case: />
      | s_index' str index == slashChar
      , s_index' str (index + 1) == closeTagChar
      = pure (Left index)

      -- case: >
      | s_index' str index == closeTagChar
      = pure (Right index)

      -- case: attr=' or attr="
      | s_index' str afterAttrName == equalChar
      , usedChar == quoteChar || usedChar == doubleQuoteChar
      = case elemIndexFrom' usedChar str (quoteIndex + 1) of
          Nothing ->
            throw
              (XenoParseError index "Couldn't find the matching quote character.")
          Just endQuoteIndex -> do
            attrF
              (substring' str index afterAttrName)
              (substring'
                 str
                 (quoteIndex + 1)
                 (endQuoteIndex))
            findAttributes (endQuoteIndex + 1)

      -- case: attr= without following quote
      | s_index' str afterAttrName == equalChar
      = throw (XenoParseError index("Expected ' or \", got: " <> S.singleton usedChar))

      | otherwise
      = throw (XenoParseError index ("Expected =, got: " <> S.singleton (s_index' str afterAttrName) <> " at character index: " <> (S8.pack . show) afterAttrName))
      where
        index = skipSpaces str index0
#ifdef WHITESPACE_AROUND_EQUALS
        afterAttrName = skipSpaces str (parseName str index)
        quoteIndex = skipSpaces str (afterAttrName + 1)
#else
        afterAttrName = parseName str index
        quoteIndex = afterAttrName + 1
#endif
        usedChar = s_index' str quoteIndex

{-# INLINE process #-}
{-# SPECIALISE process :: Process (Identity ()) -> ByteString -> Identity ()
               #-}
{-# SPECIALISE process :: Process (State s ()) -> ByteString -> State s ()
               #-}
{-# SPECIALISE process :: Process (ST s ()) -> ByteString -> ST s ()
               #-}
{-# SPECIALISE process :: Process (IO ()) -> ByteString -> IO ()
               #-}
{-# SPECIALISE process :: Process (Identity ()) -> ByteStringZeroTerminated -> Identity ()
               #-}
{-# SPECIALISE process :: Process (State s ()) -> ByteStringZeroTerminated -> State s ()
               #-}
{-# SPECIALISE process :: Process (ST s ()) -> ByteStringZeroTerminated -> ST s ()
               #-}
{-# SPECIALISE process :: Process (IO ()) -> ByteStringZeroTerminated -> IO ()
               #-}

--------------------------------------------------------------------------------
-- ByteString utilities

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
s_index :: ByteString -> Int -> Word8
s_index ps n
    | n < 0            = throw (XenoStringIndexProblem n ps)
    | n >= S.length ps = throw (XenoStringIndexProblem n ps)
    | otherwise        = ps `SU.unsafeIndex` n
{-# INLINE s_index #-}

-- | A fast space skipping function.
skipSpaces :: (StringLike str) => str -> Int -> Int
skipSpaces str i =
  if isSpaceChar (s_index' str i)
    then skipSpaces str (i + 1)
    else i
{-# INLINE skipSpaces #-}

-- | Get a substring of a string.
substring :: ByteString -> Int -> Int -> ByteString
substring s start end = S.take (end - start) (S.drop start s)
{-# INLINE substring #-}

-- | Basically @findIndex (not . isNameChar)@, but doesn't allocate.
parseName :: (StringLike str) => str -> Int -> Int
parseName str index =
  if not (isNameChar1 (s_index' str index))
     then index
     else parseName' str (index + 1)
{-# INLINE parseName #-}

-- | Basically @findIndex (not . isNameChar)@, but doesn't allocate.
parseName' :: (StringLike str) => str -> Int -> Int
parseName' str index =
  if not (isNameChar (s_index' str index))
     then index
     else parseName' str (index + 1)
{-# INLINE parseName' #-}

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+ offset) (S.elemIndex c (S.drop offset str))
-- Without the INLINE below, the whole function is twice as slow and
-- has linear allocation. See git commit with this comment for
-- results.
{-# INLINE elemIndexFrom #-}

--------------------------------------------------------------------------------
-- Character types

isSpaceChar :: Word8 -> Bool
isSpaceChar = testBit (0b100000000000000000010011000000000 :: Int) . fromIntegral
--                       |                  |  ||  bits:
--                       |                  |  |+-- 9
--                       |                  |  +--- 10
--                       |                  +------ 13
--                       +------------------------- 32
{-# INLINE isSpaceChar #-}

-- | Is the character a valid first tag/attribute name constituent?
-- 'a'-'z', 'A'-'Z', '_', ':'
isNameChar1 :: Word8 -> Bool
isNameChar1 c =
  (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95 || c == 58
{-# INLINE isNameChar1 #-}

-- isNameCharOriginal :: Word8 -> Bool
-- isNameCharOriginal c =
--   (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95 || c == 58 ||
--   c == 45 || c == 46 || (c >= 48 && c <= 57)
-- {-# INLINE isNameCharOriginal #-}
--
-- TODO Strange, but highMaskIsNameChar, lowMaskIsNameChar don't calculate fast... FIX IT
-- highMaskIsNameChar, lowMaskIsNameChar :: Word64
-- (highMaskIsNameChar, lowMaskIsNameChar) =
--     foldl (\(hi,low) char -> (hi `setBit` (char - 64), low `setBit` char)) -- NB: `setBit` can process overflowed values (where char < 64; -- TODO fix it
--           (0::Word64, 0::Word64)
--           (map fromIntegral (filter isNameCharOriginal [0..128]))
-- {-# INLINE highMaskIsNameChar #-}
-- {-# INLINE lowMaskIsNameChar #-}

-- | Is the character a valid tag/attribute name constituent?
-- isNameChar1 + '-', '.', '0'-'9'
isNameChar :: Word8 -> Bool
isNameChar char = (lowMaskIsNameChar `testBit` char'low) || (highMaskIsNameChar `testBit` char'high)
   -- TODO 1) change code to use W# instead of Word64
   --      2) Document `ii - 64` -- there is underflow, but `testBit` can process this!
  where
    char'low  = fromIntegral char
    char'high = fromIntegral (char - 64)
    highMaskIsNameChar :: Word64
    highMaskIsNameChar = 0b11111111111111111111111111010000111111111111111111111111110
    --                     ------------+------------- |    ------------+-------------
    --                                 |              |                |  bits:
    --                                 |              |                +-- 65-90
    --                                 |              +------------------- 95
    --                                 +---------------------------------- 97-122
    lowMaskIsNameChar :: Word64
    lowMaskIsNameChar =  0b11111111111011000000000000000000000000000000000000000000000
    --                     -----+----- ||
    --                          |      ||  bits:
    --                          |      |+-- 45
    --                          |      +--- 46
    --                          +---------- 48-58
{-# INLINE isNameChar #-}

-- | Char for '\''.
quoteChar :: Word8
quoteChar = 39

-- | Char for '"'.
doubleQuoteChar :: Word8
doubleQuoteChar = 34

-- | Char for '='.
equalChar :: Word8
equalChar = 61

-- | Char for '?'.
questionChar :: Word8
questionChar = 63

-- | Char for '/'.
slashChar :: Word8
slashChar = 47

-- | Exclaimation character !.
bangChar :: Word8
bangChar = 33

-- | The dash character.
commentChar :: Word8
commentChar = 45 -- '-'

-- | Open tag character.
openTagChar :: Word8
openTagChar = 60 -- '<'

-- | Close tag character.
closeTagChar :: Word8
closeTagChar = 62 -- '>'

-- | Open angle bracket character.
openAngleBracketChar :: Word8
openAngleBracketChar = 91

-- | Close angle bracket character.
closeAngleBracketChar :: Word8
closeAngleBracketChar = 93

-- | Skip initial DOCTYPE declaration
skipDoctype :: ByteString -> ByteString
skipDoctype = skipTag "<!DOCTYPE" 

-- | Skip initial ?xml declaration
skipXml :: BS.ByteString -> BS.ByteString
skipXml = skipTag "<?xml"

skipTag :: BS.ByteString -> BS.ByteString -> BS.ByteString
skipTag tag arg =
    if tag `S8.isPrefixOf` bs
      then let (_, rest)=">" `S8.breakSubstring` bs
           in skipBlanks $ S8.drop 1 rest
      else bs
  where
    bs = skipBlanks arg
    skipBlanks = S8.dropWhile isSpace
