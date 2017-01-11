-- | Shared types.

module Xeno.Types where

import Control.Exception
import Data.Typeable

data XenoException
  = XenoStringIndexProblem
  | XenoParseError
  deriving (Show, Typeable)
instance Exception XenoException where displayException = show
