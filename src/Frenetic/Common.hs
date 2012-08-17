-- |Functions and types that heavily used by the Frenetic implementation.
module Frenetic.Common
  ( Set
  , Map
  , MultiSet
  , ByteString
  , module Control.Concurrent.Chan
  , module Control.Concurrent
  , module System.Log.Logger
  , module Data.Monoid
  , select
  , both
  , mapChan
  , catMaybes
  ) where

import System.Log.Logger hiding (Priority)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad
import Data.Monoid hiding (Any)
import Data.Set (Set)
import Data.Map (Map)
import Data.MultiSet
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes)

-- |Produce a new channel that carries updates from both of the input channels,
-- but does not wait for both to be ready.  Analogous to Unix SELECT(2) followed
-- by READ(2) on the ready file descriptor.
select :: Chan a -> Chan b -> IO (Chan (Either a b))
select chan1 chan2 = do
  mergedChan <- newChan
  forkIO $ forever $ do
    v <- readChan chan1
    writeChan mergedChan (Left v)
  forkIO $ forever $ do
    v <- readChan chan2
    writeChan mergedChan (Right v)
  return mergedChan

-- |Produce a new channel that waits for both input channels to produce a value,
-- and then yields the latest version of both values.  If one channel produces
-- multiple values before the other produces any, then the early values are
-- discarded.  Afterwards, whenever one channel updates, the output channel
-- yields that update along with whatever the current version of the other
-- channel is.
both :: Chan a -> Chan b -> IO (Chan (a, b))
both chan1 chan2 = do
  merged <- select chan1 chan2
  result <- newChan
  let loop a b = do
        v <- readChan merged
        case (v, a, b) of
          (Left a, _, Nothing) -> loop (Just a) Nothing
          (Left a, _, Just b) -> do
            writeChan result (a, b)
            loop (Just a) (Just b)
          (Right b, Nothing, _) -> loop Nothing (Just b)
          (Right b, Just a, _) -> do
            writeChan result (a, b)
            loop (Just a) (Just b)
  forkIO (loop Nothing Nothing)
  return result

mapChan :: Chan a -> (a -> IO b) -> IO (Chan b)
mapChan chanA f = do
  chanB <- newChan
  forkIO $ forever $ do
    a <- readChan chanA
    b <- f a
    writeChan chanB b
  return chanB
     
