{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

-- | Shared types.

module Xeno.Types where

import Control.DeepSeq
import Control.Exception
import Data.ByteString.Char8 (ByteString, pack)
import Data.Data
import Data.Typeable
import GHC.Generics

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail

-- It is recommended to use more specific `failHere` instead
instance MonadFail (Either Xeno.Types.XenoException) where
  fail = Left . XenoParseError 0 . pack
#endif

data XenoException
  = XenoStringIndexProblem { stringIndex :: Int, inputString :: ByteString }
  | XenoParseError         { inputIndex  :: Int, message     :: ByteString }
  | XenoExpectRootNode
  deriving (Show, Data, Typeable, NFData, Generic)

instance Exception XenoException where displayException = show

