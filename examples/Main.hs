module Main where

import Prelude hiding (init)
import System.Console.GetOpt
import System.Environment
import Control.Monad
import qualified Arp
import qualified ArpSpoof
import qualified Internet
import qualified MacLearning
import qualified Query1
import qualified Query2
import qualified NetStatus
import qualified Repeater
import qualified Monitor
import qualified NAT
import qualified KitchenSink
import qualified BaseMon
import qualified PacketOut
import qualified ReserveWeb
import qualified OneRes
-- import qualified ShortestPath
import qualified TransparentCache
import qualified WireMonitor
-- import qualified ErrorDetect
-- import qualified Campus
import qualified VlanBug

import System.Log.Logger
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Formatter
import System.IO

data Arg
  = Logfile String
  | Verbosity String
  | Example (IO ())
  | Help

argSpec =
  [ Option ['l'] ["log"] (ReqArg Logfile "FILE")
      "log to FILE"
  , Option ['v'] ["verbosity"] (ReqArg Verbosity "PRIORITY")
      "sets the verbosity of the log"
  , Option ['a'] ["arp"] (NoArg (Example Arp.main))
      "an interesting controller-based ARP cache"
  , Option [] ["arpspoof"] (NoArg (Example ArpSpoof.main))
      "respond to arp requests for 10.0.0.101 with a fake mac address."
  , Option [] ["monitor"] (NoArg (Example Monitor.main))
      "monitor traffic from source IPs"
  , Option ['r'] ["repeater"] (NoArg (Example Repeater.main))
      "a simple repeater"
  , Option [] ["query1"] (NoArg (Example Query1.main))
      "a repeater that also counts packets"
  , Option [] ["query2"] (NoArg (Example Query2.main))
      "a repeater that also counts packets at switch 5 for DlSrc 1"
  , Option [] ["netstat"] (NoArg (Example NetStatus.main))
      "a repeater that monitors switch/port events"
  , Option [] ["maclearning"] (NoArg (Example MacLearning.main))
      "a learning switch"
  , Option ['s'] ["sink"] (NoArg (Example KitchenSink.main))
      "Lots of examples in slices."
  , Option [] ["internet"] (NoArg (Example Internet.main))
      "tests connecting Mininet to the Internet"
  , Option [] ["nat"] (NoArg (Example NAT.main))
      "tests connecting Mininet to the Internet through a NAT"
  , Option [] ["pktout"] (NoArg (Example PacketOut.main))
      "a controller that sends out packets"
  , Option [] ["reserve"] (NoArg (Example ReserveWeb.main))
      "reserve Web"
  , Option ['h'] ["help"] (NoArg Help) "print this help message"
  , Option [ ] ["basemon"] (NoArg (Example BaseMon.main)) "Basic Monitoring"
  , Option [] ["oneres"] (NoArg (Example OneRes.main)) 
       "Constructs a one resiliant routing policy for a network"
--   , Option [] ["sp"] (NoArg (Example ShortestPath.main))
--       "runs the shortest path algorithm"
  , Option [] ["tc"] (NoArg (Example TransparentCache.main))
      "Configures a single switch to divert traffic to a transparent cache."
  , Option [] ["wm"] (NoArg (Example WireMonitor.main))
      "a simple wire with monitoring"
--   , Option [] ["errordetectb"] (NoArg (Example ErrorDetect.main))
--       "Error detection for a basic, static topology"
--   , Option [] ["campus"] (NoArg (Example Campus.main))
--       "A small example of a campus network with several slices."
  , Option [] ["vlanbug"] (NoArg (Example VlanBug.main))
      "Demonstrate a bug with how OpenVSwitch handles VLAN tags."
  ]

init [Help] = putStrLn (usageInfo "Usage Info" argSpec)
init (Verbosity s : rest) = case s of
  "DEBUG" -> setLog DEBUG rest
  "INFO" -> setLog INFO rest
  "ERROR" -> setLog ERROR rest
  otherwise -> fail $ "invalid value " ++ s
init [] = fail "too few arguments"
init rest = setLog INFO rest

myLogFormatter = simpleLogFormatter "[$prio : $time : $loggername] $msg"

setLogger prio handler = do
  logger <- getRootLogger
  let handler' = setFormatter handler myLogFormatter
  let logger' = setHandlers [handler'] logger
  saveGlobalLogger (setLevel prio logger')

setLog logPriority ((Logfile path) : rest) = do
  handler <- fileHandler path logPriority
  setLogger logPriority handler
  start rest
setLog logPriority rest = do
  handler <- streamHandler stderr logPriority
  setLogger logPriority handler
  start rest

start [Example proc] = proc
start [] = fail "too few arguments"
start _ =  fail "too many arguments"

main = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do
    mapM_ putStrLn errors
    fail "invalid arguments"
  init args
