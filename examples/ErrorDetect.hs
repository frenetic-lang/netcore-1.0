module ErrorDetect where

import System.IO
import System.IO.Unsafe

import Data.Word
import Data.Map as Map
import Data.IORef
import Data.Graph.Inductive

import Control.Concurrent
import Control.Monad

import Frenetic.NetCore
import Frenetic.Topo

import Repeater


--mininet's default topology
topo=buildGraph [((2,0), (1,1)), ((3,0), (1,2))] 

--a map of counters keyed by node ids
countTable :: IORef (Map Int Int) 
countTable = unsafePerformIO (newIORef Map.empty)

--a map of query actions and channels to read to get info about the query
monTable :: IORef (Map Int (Chan (Switch, Integer), [Action]))
monTable = unsafePerformIO (newIORef Map.empty)


buildCTable :: IO ()
buildCTable = do --build the node counter map and initialize counters to 0
  readIORef countTable >>= print
  t <- readIORef countTable 
  writeIORef countTable ((Prelude.foldl (\acc x -> insert x 0 acc)) t (nodes topo))
  readIORef countTable >>= print

insertMonTable :: (Int,IO (Chan (Switch, Integer), [Action])) -> IO ()
insertMonTable (ind,c) = do
  (channel, qa) <- c
  modifyIORef monTable (insert ind (channel, qa))
  return ()

buildMonTable :: [Int] -> IO ()
buildMonTable ns = 
  case ns of 
               (x:xs) -> do 
                              insertMonTable (x,(countPkts 1000))
                              buildMonTable xs
               [] -> do return () 
  

main = do
  buildCTable
  let ns = (nodes topo)
  buildMonTable ns
  t <- readIORef monTable
  (c, q) <- countPkts 1000
  let p = Prelude.foldl (\acc x -> PoUnion acc 
          (DlDst (ethernetAddress 0 0 0 0 0 (fromIntegral x)) ==> 
            snd (findWithDefault (c,q) x t))) PoBottom (nodes topo) 
  --todo: make this a check for predicates on counter for error detection
  forkIO $ forever $ do 
    (sw, n) <- readChan (fst (findWithDefault (c,q) 2 t))
    putStrLn ("Packts to " ++ show sw ++ ": " ++  show n)
    hFlush stdout
  controller (Repeater.policy `PoUnion` p)

