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
  , bothChan
  , catMaybes
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
import Data.Maybe (catMaybes)

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

bothChan :: Chan a -> Chan b -> IO (Chan (a, b))
bothChan chan1 chan2 = do
  merged <- mergeChan chan1 chan2
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
