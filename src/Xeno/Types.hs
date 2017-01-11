-- | Shared types.

module Xeno.Types where

import Control.Exception
import Data.Typeable

data XenoException
  = XenoStringIndexProblem
  | XenoParseError
  | XenoExpectRootNode
  deriving (Show, Typeable)
instance Exception XenoException where displayException = show
