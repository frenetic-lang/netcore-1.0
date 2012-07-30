module Main where

import Prelude hiding (init)
import System.Console.GetOpt
import System.Environment
import Control.Monad
import qualified Repeater
import qualified Query1
import qualified MacLearning
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
  , Option ['r'] ["repeater"] (NoArg (Example Repeater.main)) 
      "a simple repeater"
  , Option [] ["query1"] (NoArg (Example Query1.main)) 
      "a repeater that also counts packets"
  , Option [] ["maclearning"] (NoArg (Example MacLearning.main))
      "a learning switch"
  , Option ['h'] ["help"] (NoArg Help) "print this help message"
  ]

init [Help] = do
  putStrLn (usageInfo "Usage Info" argSpec)
init (Verbosity s : rest) = case s of
  "DEBUG" -> setLog DEBUG rest
  "INFO" -> setLog INFO rest
  otherwise -> do
    fail $ "invalid value " ++ s
init [] = fail "too few arguments"
init rest = do
  setLog INFO rest

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

start [Example proc] = do
  proc
start [] = fail "too few arguments"
start _ =  fail "too many arguments"

main = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do
    mapM_ putStrLn errors
    fail "invalid arguments"
  init args
  