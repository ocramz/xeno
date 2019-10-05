{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

-- | SAX parser and API for XML.

module Xeno.SAX.CPS
  ( cps
  ) where

import           Control.Exception
import           Control.Monad.State.Strict
import           Control.Monad.ST.Strict
import           Control.Spork
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Unsafe as SU
import           Data.Char(isSpace)
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Word
import           Xeno.Types
--import           Data.Mutable
import           Data.STRef
{-import           Data.Vector.Unboxed      ((!))
import qualified Data.Vector.Unboxed as UV -}
import qualified Data.Vector.Unboxed.Mutable as UMV

import Xeno.SAX

cps ::       Process (ST s b)            -- initial processor
    -> ST s (Process (ST s b),           -- mutable processor
             Process (ST s b) -> ST s () -- assign next processor
            )
cps p = do
    current <- newSTRef p
    let next anotherProcess = writeSTRef current anotherProcess
    return (Process {
        openF    = wrap1 current openF
      , endOpenF = wrap1 current endOpenF
      , closeF   = wrap1 current closeF
      , textF    = wrap1 current textF
      , cdataF   = wrap1 current cdataF
      , attrF    = wrap2 current attrF
      }, next)

--wrap1 :: (t -> a ->      ST s b) -> a ->      ST s b
wrap1 current aField arg = do
  rec <- readSTRef current
  aField rec arg

--wrap2 :: (t -> a -> b -> ST s c) -> a -> b -> ST s c
wrap2 current aField arg1 arg2 = do
  rec <- readSTRef current
  aField rec arg1 arg2

{-
pushdown p = do
  (proc, next) <- cps p
  stackSize  <- newSTRef 0
  stackAlloc <- newSTRef 16
  stack      <- 
  let call aProc = do
-}
