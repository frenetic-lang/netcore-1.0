module Frenetic.TopoGen
  ( linear
  , linearHosts
  , kComplete
  , kCompleteHosts
  , waxman
  , smallworld
  , fattree
  ) where

import Data.Graph.Inductive.Graph
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import System.Random

import Frenetic.NetCore.Types
import Frenetic.Topo

distance (x1, y1) (x2, y2) = ((x1 - x2) ** 2 + (y1 - y2) ** 2) ** 0.5

rand01IO :: IO Double
rand01IO = randomRIO (0.0, 1.0)

-- |Take a list of linked nodes, and connect them via ports that share the name
-- of their destination node.  So [(1, 2)] becomes [((1, 2), (2, 1))]
pairsToLinks :: [(Node, Node)] -> [((Node, Port), (Node, Port))]
pairsToLinks = map pairToLink

pairToLink :: (Node, Node) -> ((Node, Port), (Node, Port))
pairToLink (n1, n2) = ((n1, fromIntegral n2), (n2, fromIntegral n1))

-- |Adds n hosts to each node, numbered as 100 * Node + i where 1 <= i <= n, and
-- each node links to the host with a port numbered the same as the name of the
-- host.
buildHosts :: (Integral n) => n -> [Node] -> [((Node, Port), (Node, Port))]
buildHosts n nodes = concat . map addHosts $ nodes where
  n' = fromIntegral n :: Int
  addHosts node = [ ((node, fromIntegral host), (host, 0))
                  | host <- [100 * node + 1 .. 100 * node + n']]

addHosts :: (Integral n) => n -> [(Node, Node)] ->
                                 [((Node, Port), (Node, Port))]
addHosts n edges = edges' ++ hosts where
  (firsts, seconds) = unzip edges
  nodes = Set.toList . Set.fromList $ firsts ++ seconds
  edges' = pairsToLinks edges
  hosts = buildHosts n nodes

-- | Produce a topology with n nodes in a row, port 1 pointing to the previous
-- and port 2 pointing to the next, starting at node 0.
linear :: (Integral n) => n -> Topo
linear n = graph where
  nodes = [0 .. ((fromIntegral n - 1))]
  pairs = zip nodes (tail nodes)
  graph = buildGraph . map (\(n1, n2) -> ((n1, 2), (n2, 1))) $ pairs

-- | Produce a topology with n nodes in a row, port 1 pointing to the previous
-- and port 2 pointing to the next, starting at node 0.  Hosts are numbered 100
-- + (10 * switch) and 101 + (10 * switch).  Does not support more than 10
-- switches.  Port 3 connects to host XX0, port 4 connects to host XX1.
linearHosts :: (Integral n) => n -> Topo
linearHosts n = graph where
  nodes = [0 .. (fromIntegral n - 1)]
  hostLinks = concatMap buildHosts nodes
  pairs = zip nodes (tail nodes)
  graph = buildGraph $ (map (\(n1, n2) -> ((n1, 2), (n2, 1))) pairs) ++ hostLinks
  buildHosts node = [((node, 3), (100 + 10 * node, 0)),
                     ((node, 4), (101 + 10 * node, 0))]

kComplete :: (Integral n) => n -> Topo
-- |Produce a topology on the n-complete graph, starting with node 1.  Each node
-- x has an edge to node y over port y.
kComplete n = buildGraph pairs where
  nodes :: [Node]
  nodes = [1 .. (fromIntegral n)]
  pairs = [((fromIntegral x, (fromIntegral y)),
            (fromIntegral y, (fromIntegral x)))
          | (x:xs) <- List.tails nodes, y <- xs]

-- |Produce a topology on the n-complete graph, starting with node 1.  Each node
-- x has an edge to node y over port y, and two hosts 1i1 and 1i2 connected at
-- ports of the same name.
kCompleteHosts :: (Integral n) => n -> Topo
kCompleteHosts n = buildGraph $ hostLinks ++ pairs where
  nodes :: [Node]
  nodes = [1 .. (fromIntegral n)]
  hostLinks = concat . map buildHostLinks $ nodes
  pairs = [((fromIntegral x, (fromIntegral y)),
            (fromIntegral y, (fromIntegral x)))
          | (x:xs) <- List.tails nodes, y <- xs]
  buildHostLinks i = [ ((i, fromIntegral h1), (h1, 0))
                     , ((i, fromIntegral h2), (h2, 0)) ] where
    h1 = 101 + 10 * i
    h2 = 102 + 10 * i

-- |Build a Waxman random graph with n nodes, h hosts on each node, and
-- parameters a and b.  Suggested parameters:  a=0.8, b=0.18
waxman :: (Integral n, Integral h) => n -> h -> Double -> Double -> IO Topo
waxman n h a b = do
  plotted <- sequence [ do
                        x <- rand01IO
                        y <- rand01IO
                        return (n, (x, y))
                      | n <- ns]
  let distances = [ distance p1 p2
                  | (_, p1) <- plotted, (_, p2) <- plotted]
  let maxDistance = maximum distances
  let link (i, ip) (j, jp) = do
        let d = distance ip jp
        rv <- rand01IO
        if rv < a * (exp (-d / (b * maxDistance))) then return $ Just (i, j)
                                             else return Nothing
  edges <- sequence [ link i j | i : js <- List.tails plotted, j <- js]
  let edges' = Maybe.catMaybes edges
  let links = addHosts h edges'
  return $ buildGraph links
  where
  n' = fromIntegral n :: Node
  ns = [1 .. n']

-- |Build a Watts-Strogatz graph on n nodes, with degree k (even, n >> k >>
-- ln(n) >> 1) and rewiring likelihood 0 <= b <= 1.  See
-- en.wikipedia.org/wiki/Watts_and_Strogatz_model for more information, and the
-- algorithm we follow.  Suggested parameters: k = n/3, b=0.3
smallworld :: (Integral n, Integral h, Integral k) =>
              n -> h -> k -> Double -> IO Topo
smallworld n h k b = do
  edges' <- sequence $ map rewire edges
  let edges'' = Set.toList . Set.fromList $ edges'
  return $ buildGraph (addHosts h edges'')
  where
  n' = fromIntegral n :: Node
  ns = [1 .. n']
  -- Get the edges in the ring forcing i < j
  edges = [ (i, j)
          | i <- ns, j <- [i + 1 .. n'] -- => i < j => abs(i - j) > 0
          , abs (i - j) <= fromIntegral k `quot` 2]
  rewire :: (Node, Node) -> IO (Node, Node)
  rewire (i, j) = do
    let options = [x | x <- ns, x /= i]
    rwValue <- rand01IO
    let rw = rwValue < b
    if rw then do
      index <- randomRIO (0, length options - 1)
      let j' = options !! index
      return (i, j')
    else return (i, j)

-- |Build a fattree topology with two aggregating switches, four top-of-rack
-- switches and six hosts per rack.
fattree :: Topo
fattree = buildGraph links where
  a1 = 1
  a2 = 2
  t1 = 11
  t2 = 12
  t3 = 13
  t4 = 14
  links = [
            ((a1, 1), (t1, 1))
          , ((a2, 1), (t1, 2))
          , ((a1, 2), (t2, 1))
          , ((a2, 2), (t2, 2))
          , ((a1, 3), (t3, 1))
          , ((a2, 3), (t3, 2))
          , ((a1, 4), (t4, 1))
          , ((a2, 4), (t4, 2))
          ] ++ hosts
  hosts = [ ((t, fromIntegral h), (h, 0))
          | t <- [t1, t2, t3, t4], h <- [t * 10 .. t * 10 + 5]]
