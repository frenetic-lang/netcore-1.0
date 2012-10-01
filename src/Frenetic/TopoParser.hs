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
 -  This parses a string in that format into [(switch1, [hosts...]), (switch2,
 -  [hosts...]), ...]
 -  .
 -    -}
topoEdgeList :: GenParser Char st [(Int, [String])]
topoEdgeList = 
     do result <- many topoLine
        eof
        return result 

topoLine :: GenParser Char st (Int, [String])
topoLine =
    do result <- host
       eol 
       return result

host :: GenParser Char st (Int, [String]) 
host = 
  do first <- switch
     string "<->"
     next <- neighbor
     return (first, next)

switch :: GenParser Char st Int
switch = do
  d <- many1 digit
  return (read d)

neighbor :: GenParser Char st [String]
neighbor = 
    (char ' ' >> neighbor ) --found neighbor
    <|> (return []) --is a singleton (or at the end)

eol :: GenParser Char st Char
eol = char '\n'

--to make this the type we really want should do an additional map:
--to pick off index [1] of all of the strings in the return type of
--parseTopo and cast that to an int (i.e. to a node)
parseTopo :: String -> Either ParseError [(Int, [String])]
parseTopo t = parse topoEdgeList "(unknown)" t


