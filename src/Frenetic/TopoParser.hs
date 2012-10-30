module Frenetic.TopoParser
  ( parseTopo 
  , makeEdgeList
  ) where

import Text.ParserCombinators.Parsec
import Frenetic.Topo as Topo
import Data.List
import Data.Maybe
import Data.Word
import Frenetic.NetCore.Types

{- A topology is represented by mininet as a string of the following format:
 -  switch <-> host-ethaddr host-ethaddr ...
 -  switch <-> hosto-ethaddr .....
 -  .
 -  This parses a string in that format into [(switch1, [hosts...]), (switch2,
 -  [hosts...]), ...]
 -  .
 -    -}
topoEdgeList :: GenParser Char st [((Int,Char), [(Int,Char)])]
topoEdgeList = 
     do result <- many topoLine
        eof
        return result 

topoLine :: GenParser Char st ((Int,Char), [(Int,Char)])
topoLine =
    do s <- switch 
       string " <-> "
       nbs <- nbsl
       eol 
       return (s, nbs) 

switch :: GenParser Char st (Int, Char)
switch = do
  s <- satisfy (\x -> x == 's')
  d <- many1 digit
  return ((read d),s)

nbsl :: GenParser Char st [(Int,Char)]
nbsl = 
  do first <- fneighbor
     next <- neighbor
     return (first : next)

fneighbor :: GenParser Char st (Int, Char)
fneighbor = 
  do l <- satisfy (\x -> x == 'h' || x == 's') 
     d <- many1 digit
     return ((read d), l)

neighbor :: GenParser Char st [(Int,Char)]
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
parseTopo :: String -> Either ParseError [((Int,Char), [(Int,Char)])]
parseTopo t = parse topoEdgeList "(unknown)" t

makeEdgeList :: [((Int,Char), [(Int,Char)])] -> [((Element, Port), (Element, Port))]
makeEdgeList l = foldl (\x -> \y -> (sList y l) `union` x) [] l

sList :: ((Int,Char), [(Int,Char)]) -> [((Int,Char), [(Int,Char)])] -> [((Element, Port), (Element, Port))]
sList (s, ns) l = fst 
  (foldl 
    (\x y -> 
      (((toElement s, (fromIntegral (snd x) :: Word16)), 
        (toElement y, findPort y s ((filter (\w -> (fst w) ==  y) l) !! 0)))
        : (fst x) , (snd x)+1))
  ([], 1) ns)

toElement :: (Int, Char) -> Element
toElement (l, hors) = if hors == 'h' then Host (fromIntegral l :: Word64) 
                                     else Topo.Switch (fromIntegral l :: Word64)

--sList (s,ns) l = fst (foldl (\x y -> ((m,snd x), (fst y, port y s 

findPort :: (Int,Char) -> (Int,Char) -> ((Int,Char), [(Int,Char)]) -> Port 
findPort a b l = if snd a == 'h' 
                  then (fromIntegral 0 :: Word16)
                  else (fromIntegral ((fromJust (elemIndex b (snd l)))+1) :: Word16)




