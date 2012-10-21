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


buildCTable :: IO ()
buildCTable = do --build the node counter map and initialize counters to 0
  readIORef countTable >>= print
  t <- readIORef countTable 
  writeIORef countTable ((Prelude.foldl (\acc x -> insert x 0 acc)) t (nodes topo))
  readIORef countTable >>= print


main = do
  buildCTable
  let chanl = (Prelude.foldl (\acc x -> (countPkts 1000) : acc) [] (nodes topo))
  --let p = Prelude.foldl (\acc x -> PoUnion acc (DlDst (ethernetAddress 0 0 0 0 0 x) ==> 
  --        (snd (chanl !! (x-1))))) PoBottom (nodes.topo) --agh monads!
  --make this a fold to create channels reading counters for all nodes
  (c, qa) <- countPkts 1000
  let p = DlDst (ethernetAddress 0 0 0 0 0 2) ==> qa

  --todo: make this a check for predicates on counters
  forkIO $ forever $ do 
    (sw, n) <- readChan c
    putStrLn ("Packts to h2: " ++ show n)
    hFlush stdout
  controller (Repeater.policy `PoUnion` p)

