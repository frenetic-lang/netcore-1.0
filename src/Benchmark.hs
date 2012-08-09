module Main (main) where

import Prelude hiding (init)
import Control.Monad
import qualified Data.Map as Map
import Frenetic.NetCore
import Frenetic.NetCore.Pretty
import Frenetic.NetCore.Types (size)
import Frenetic.TopoGen
import Frenetic.PolicyGen
import Frenetic.Slices.Compile
import Frenetic.Slices.VlanAssignment
import Frenetic.Slices.Slice
import Frenetic.Slices.Sat
import System.Console.GetOpt
import System.CPUTime
import System.Environment
import System.Exit
import Text.Printf

data GraphType = Fattree | Smallworld | Waxman
data PolicyType = ShortestPath | Multicast

data Options = Options {
  optEdge :: Bool
, optAST :: Bool
, optTime :: Bool
, optIso :: Bool
, optComp :: Bool
, optGraph :: GraphType
, optPolicy :: PolicyType
, optNodes :: Int
, optHosts :: Int
}

defaultOptions :: Options
defaultOptions = Options {
  optEdge = False
, optAST = False
, optTime = True
, optIso = False
, optComp = False
, optGraph = Smallworld
, optPolicy = ShortestPath
, optNodes = 20
, optHosts = 1
}

options :: [OptDescr (Options -> IO Options)]
options =
  [
    Option ['e'] ["edge"] (NoArg setEdge) "Use edge compiler"

  , Option ['a'] ["ast"] (NoArg setAST) "Measure AST size"
  , Option ['l'] ["time"] (NoArg setTime) "Time compilation"
  , Option ['i'] ["isolation"] (NoArg setIso) "Time isolation validation"
  , Option ['c'] ["compilation"] (NoArg setComp) "Time compilation validation"

  , Option ['f'] ["fattree"] (NoArg setFattree) "Use fattree topology."
  , Option ['s'] ["smallworld"] (NoArg setSmallworld) "Use small world random topology."
  , Option ['w'] ["waxman"] (NoArg setWaxman) "Use Waxman random topology."
  , Option ['p'] ["shortest"] (NoArg setShortest) "Use shortest path routing."
  , Option ['m'] ["multicast"] (NoArg setMulticast) "Use multicast routing."
  , Option ['n'] ["nodes"] (ReqArg readNodes "NODES") "Number of nodes to use."
  , Option ['t'] ["hosts"] (ReqArg readHosts "HOSTS") "Number of hosts per switch."
  , Option ['h'] ["help"]  (NoArg showHelp) "print this help message"
  ]

setEdge :: Options -> IO Options
setEdge opt = return opt { optEdge = True }

setAST :: Options -> IO Options
setAST opt = return opt { optAST = True }

setTime :: Options -> IO Options
setTime opt = return opt { optTime = True }

setIso :: Options -> IO Options
setIso opt = return opt { optIso = True }

setComp :: Options -> IO Options
setComp opt = return opt { optComp = True }

setFattree :: Options -> IO Options
setFattree opt = return opt { optGraph = Fattree }

setSmallworld :: Options -> IO Options
setSmallworld opt = return opt { optGraph = Smallworld }

setWaxman :: Options -> IO Options
setWaxman opt = return opt { optGraph = Waxman }

setMulticast :: Options -> IO Options
setMulticast opt = return opt { optPolicy = Multicast }

setShortest :: Options -> IO Options
setShortest opt = return opt { optPolicy = ShortestPath }

readNodes :: String -> Options -> IO Options
readNodes arg opt = return opt { optNodes = read arg }

readHosts :: String -> Options -> IO Options
readHosts arg opt = return opt { optHosts = read arg }

showHelp _ = do
  putStrLn (usageInfo "Usage Info" options)
  exitWith ExitSuccess

doWaxman Options {optNodes = nodes, optHosts = hosts} =
  waxman nodes hosts 0.8 0.18

doSmallworld Options {optNodes = nodes, optHosts = hosts} =
  smallworld nodes hosts (max 4 (nodes `quot` 3)) 0.3

doFattree _ = return fattree

start options = do
  g <- case optGraph options of
         Fattree -> doFattree
         Smallworld -> doSmallworld
         Waxman -> doWaxman
       $ options
  let policy = case optPolicy options of
                 ShortestPath -> shortestPath
                 Multicast -> multicast
               $ g
  let slice = (simpleSlice g matchNone) {egress = Map.empty}
  let (compiled1, compiled2) =
        if optEdge options then
          -- Force them to be distinct by adding PoBottom to them.  Edge
          -- compilation relies on distinct (slice, policy) pairs.
          let combined = [(slice, policy), (slice, policy <+> PoBottom)] in
          let tagged = edge g combined in
          let [c1, c2] = map (\(assignment, (slice, policy)) ->
                               edgeCompileSlice slice assignment policy)
                             tagged in
          (c1, c2)
        else
          let c1 = compileSlice slice 1 policy in
          let c2 = compileSlice slice 2 policy in
          (c1, c2)
  if optTime options then do
    start <- getCPUTime
    let s = size compiled1
    s `seq` return ()
    finish <- getCPUTime
    let diff = (fromIntegral (finish - start)) / (10^12)
    printf "Compilation time: %0.3f sec\n" (diff :: Double)
  else return ()
  let pSize = size policy
  let cSize = size compiled1
  if optAST options then
    putStrLn $ "AST Size: " ++ show pSize ++ " -> " ++ show cSize
  else return ()
  if optIso options then do
    start <- getCPUTime
    sep <- separate g compiled1 compiled2
    if not sep then
      error "Not separate!"
    else return ()
    finish <- getCPUTime
    let diff = (fromIntegral (finish - start)) / (10^12)
    printf "Isolation time:   %0.3f sec\n" (diff :: Double)
  else return ()
  if optComp options then do
    start <- getCPUTime
    sep <- compiledCorrectly g slice policy compiled1
    if not sep then
      error "Not correct!"
    else return ()
    finish <- getCPUTime
    let diff = (fromIntegral (finish - start)) / (10^12)
    printf "Correctness time: %0.3f sec\n" (diff :: Double)
  else return ()


main = do
  rawArgs <- getArgs
  let (args, unmatched, errors) = getOpt RequireOrder options rawArgs
  opts <- foldl (>>=) (return defaultOptions) args
  unless (null errors) $ do
    mapM_ putStrLn errors
    fail "invalid arguments"
  start opts
