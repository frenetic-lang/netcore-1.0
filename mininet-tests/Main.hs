module Main where

import System.Console.GetOpt
import System.Environment
import Control.Monad
import qualified Repeater

data Arg
  = Example (IO ())
  | Help

argSpec =
  [ Option ['r'] ["repeater"] (NoArg (Example Repeater.main)) 
      "a simple repeater"
  , Option ['h'] ["help"] (NoArg Help) "print this help message"
  ]

action [Help] = do
  putStrLn (usageInfo "Usage Info" argSpec)
action [Example proc] = do
  proc
action [] = do
  putStrLn (usageInfo "Usage Info" argSpec)
  fail "too few arguments"
action _ = do
  putStrLn (usageInfo "Usage Info" argSpec)
  fail "too many arguments"

main = do
  rawArgs <- getArgs
  let (args, options, errors) = getOpt RequireOrder argSpec rawArgs
  unless (null errors) $ do
    mapM_ putStrLn errors
    fail "invalid arguments"
  action args
  