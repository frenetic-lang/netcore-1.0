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
  , mergeChan
  ) where

import System.Log.Logger hiding (Priority)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Data.Set (Set)
import Data.Map (Map)
import Data.MultiSet
import Data.ByteString.Lazy (ByteString)

mergeChan :: Chan a -> Chan b -> IO (Chan (Either a b))
mergeChan chan1 chan2 = do
  mergedChan <- newChan
  forkIO $ forever $ do
    v <- readChan chan1
    writeChan mergedChan (Left v)
  forkIO $ forever $ do
    v <- readChan chan2
    writeChan mergedChan (Right v)
  return mergedChan
