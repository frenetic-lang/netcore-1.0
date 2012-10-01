module Frenetic.TopoParser
  ( 

  ) where

import Text.ParserCombinators.Parsec
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Maybe

{- A topology is represented by mininet as a string of the following format:
 -  switch <-> host-ethaddr host-ethaddr ...
 -  switch <-> hosto-ethaddr .....
 -  .
 -  .
 -  .
 -    -}
topoEdgeList :: GenParser Char st [(String, [String])]
topoEdgeList = 
     do result <- many topoLine
        eof
        return result 

topoLine :: GenParser Char st (String, [String])
topoLine =
    do result <- host
       eol 
       return result

host :: GenParser Char st (String, [String]) 
host = 
  do first <- switch
     next <- neighbor
     return (first, next)

switch :: GenParser Char st String 
switch = many (noneOf "<->")

neighbor :: GenParser Char st [String]
neighbor = 
    (char ' ' >> neighbor ) --found neighbor
    <|> (return []) --is a singleton

eol :: GenParser Char st Char
eol = char '\n'

parseTopo :: String -> Either ParseError [(String, [String])]
parseTopo t = parse topoEdgeList "(unknown)" t

