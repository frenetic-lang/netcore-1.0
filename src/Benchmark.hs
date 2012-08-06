module Main (main) where

import Prelude hiding (init)
import Frenetic.NetCore
import Frenetic.NetCore.Pretty
import Frenetic.TopoGen
import Frenetic.PolicyGen
import Frenetic.Slices.Compile
import Frenetic.Slices.Slice
import Frenetic.Slices.Sat
import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Monad

data GraphType = Fattree | Smallworld | Waxman
data PolicyType = ShortestPath | Multicast

data Options = Options {
  optGraph :: GraphType
, optPolicy :: PolicyType
, optNodes :: Int
, optHosts :: Int
}

defaultOptions :: Options
defaultOptions = Options {
  optGraph = Fattree
, optPolicy = Multicast
, optNodes = 20
, optHosts = 1
}

options :: [OptDescr (Options -> IO Options)]
options =
  [
    Option ['f'] ["fattree"] (NoArg setFattree) "Use fattree topology."
  , Option ['s'] ["smallworld"] (NoArg setSmallworld) "Use small world random topology."
  , Option ['w'] ["waxman"] (NoArg setWaxman) "Use Waxman random topology."
  , Option ['p'] ["shortest"] (NoArg setShortest) "Use shortest path routing."
  , Option ['m'] ["multicast"] (NoArg setMulticast) "Use multicast routing."
  , Option ['n'] ["nodes"] (ReqArg readNodes "NODES") "Number of nodes to use."
  , Option ['t'] ["hosts"] (ReqArg readHosts "HOSTS") "Number of hosts per switch."
  , Option ['h'] ["help"]  (NoArg showHelp) "print this help message"
  ]

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
  let slice = simpleSlice g (dlDst broadcastAddress)
  let compiled = compileSlice slice 1 policy
  correct <- compiledCorrectly g slice policy compiled
  print correct

main = do
  rawArgs <- getArgs
  let (args, unmatched, errors) = getOpt RequireOrder options rawArgs
  opts <- foldl (>>=) (return defaultOptions) args
  unless (null errors) $ do
    mapM_ putStrLn errors
    fail "invalid arguments"
  start opts
  
