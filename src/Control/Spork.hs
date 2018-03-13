-- | Like the spoon package, but for catching one specific exception
-- type and returning it.

module Control.Spork
    ( spork
    ) where

import Control.Exception
import System.IO.Unsafe

-- | Evaluate `a` and return left if it throws a pure exception.
spork
  :: Exception e
  => a -> Either e a
spork a =
  unsafePerformIO $
  (Right `fmap` evaluate a) `catches` [Handler (\e -> pure (Left e))]
{-# INLINEABLE spork #-}
