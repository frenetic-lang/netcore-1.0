module Frenetic.TopoParser
  ( parseTopo 

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
topoEdgeList :: GenParser Char st [(Node, [Node])]
topoEdgeList = 
     do result <- many topoLine
        eof
        return result 

topoLine :: GenParser Char st (Node, [Node])
topoLine =
    do s <- switch 
       string " <-> "
       nbs <- nbsl
       eol 
       return (s, nbs) 

switch :: GenParser Char st Node
switch = do
  string "s"
  d <- many1 digit
  return (read d)

nbsl :: GenParser Char st [Node]
nbsl = 
  do first <- fneighbor
     next <- neighbor
     return (first : next)

fneighbor :: GenParser Char st Node
fneighbor = 
  do ((string "s") <|> (string "h"))
     d <- many1 digit
     return (read d)

neighbor :: GenParser Char st [Node]
neighbor = 
  do string "-eth"
     many1 digit
     ((char ' ' >> nbsl)
      <|> (return [])) --is a singleton (or at the end)

eol :: GenParser Char st Char
eol = char '\n'

--to make this the type we really want should do an additional map:
--to pick off index [1] of all of the strings in the return type of
--parseTopo and cast that to an int (i.e. to a node)
parseTopo :: String -> Either ParseError [(Node, [Node])]
parseTopo t = parse topoEdgeList "(unknown)" t


