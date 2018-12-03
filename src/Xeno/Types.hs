{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

-- | Shared types.

module Xeno.Types where

import Control.DeepSeq
import Control.Exception
import Data.ByteString (ByteString)
import Data.Data
import Data.Typeable
import GHC.Generics

#ifdef MIN_VERSION_base(4,9,0)
import Control.Monad.Fail.MonadFail

-- It is recommended to use more specific `failHere` instead
instance MonadFail (Either Xeno.Types.XenoException) where
  fail = Left . XenoParseError 0
#endif

data XenoException
  = XenoStringIndexProblem { stringIndex :: Int, inputString :: ByteString }
  | XenoParseError         { inputIndex  :: Int, message     :: ByteString }
  | XenoExpectRootNode
  deriving (Show, Data, Typeable, NFData, Generic)

instance Exception XenoException where displayException = show

