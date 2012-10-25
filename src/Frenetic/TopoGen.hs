module Frenetic.TopoGen
  ( linear
  , linearHosts
  , kComplete
  , kCompleteHosts
  , waxman
  , smallworld
  , fattree
  ) where

import Frenetic.Topo
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import System.Random
import Frenetic.NetCore.Types hiding (Switch)

distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

rand01IO :: IO Double
rand01IO = randomRIO (0.0, 1.0)

-- | Produce a topology with n nodes in a row, port 1 pointing to the previous
-- and port 2 pointing to the next, starting at node 0.
linear :: (Integral n) => n -> Graph
linear n = graph where
  switches = [0 .. ((fromIntegral n - 1))]
  pairs = zip switches (tail switches)
  graph = buildGraph . map (\(n1, n2) -> ((Switch n1, 2), (Switch n2, 1))) $ pairs

-- | Produce a topology with n nodes in a row, port 1 pointing to the previous
-- and port 2 pointing to the next, starting at node 0.  Hosts are numbered 100
-- + (10 * switch) and 101 + (10 * switch).  Does not support more than 10
-- switches.  Port 3 connects to host XX0, port 4 connects to host XX1.
linearHosts :: (Integral n) => n -> Graph
linearHosts n = graph where
  switches = [0 .. n - 1]
  hostLinks = concatMap buildHosts switches
  pairs = zip switches (tail switches)
  graph = buildGraph $ 
          map (\(n1, n2) -> ((Switch $ fromIntegral n1, 2), 
                             (Switch $ fromIntegral n2, 1))) 
          pairs ++ hostLinks
  buildHosts s = [((Switch $ fromIntegral s, 3), (Host $ fromIntegral $ 100 + 10 * s, 0)),
                  ((Switch $ fromIntegral s, 4), (Host $ fromIntegral $ 101 + 10 * s, 0))]

kComplete :: (Integral n) => n -> Graph
-- |Produce a topology on the n-complete graph, starting with node 1.  Each node
-- x has an edge to node y over port y.
kComplete n = buildGraph pairs where
  switches = [1 .. n] 
  pairs = [((Switch $ fromIntegral x, fromIntegral y),
            (Switch $ fromIntegral y, fromIntegral x))
          | (x:xs) <- List.tails switches, y <- xs]

-- |Produce a topology on the n-complete graph, starting with node 1.  Each node
-- x has an edge to node y over port y, and two hosts 1i1 and 1i2 connected at
-- ports of the same name.
kCompleteHosts :: (Integral n) => n -> Graph
kCompleteHosts n = buildGraph $ hostLinks ++ pairs where
  switches = [1 .. n]
  pairs = [((Switch $ fromIntegral x, fromIntegral y),
            (Switch $ fromIntegral y, fromIntegral x))
          | (x:xs) <- List.tails switches, y <- xs]
  hostLinks = concatMap buildHostLinks $ switches
  buildHostLinks s = [ ((Switch $ fromIntegral s, fromIntegral h1), (Host $ fromIntegral h1, 0))
                     , ((Switch $ fromIntegral s, fromIntegral h2), (Host $ fromIntegral h2, 0)) ] 
    where h1 = 101 + 10 * s
          h2 = 102 + 10 * s

-- |Build a Waxman random graph with n nodes, h hosts on each node, and
-- parameters a and b.  Suggested parameters:  a=0.8, b=0.18
waxman :: (Integral n, Integral h) => n -> h -> Double -> Double -> IO Graph
waxman n h a b = do
  plotted <- sequence [ do
                        x <- rand01IO
                        y <- rand01IO
                        return (n, (x, y))
                      | n <- ns]
  let maxDistance = maximum $
                    [ distance p1 p2
                    | (_, p1) <- plotted, (_, p2) <- plotted]
  let link (i, ip) (j, jp) = do
        let d = distance ip jp
        rv <- rand01IO
        if rv < a * (exp (-d / (b * maxDistance))) 
          then return $ Just (i, j) 
          else return Nothing        
  edges <- sequence [ link i j | i : js <- List.tails plotted, j <- js]
  let edges' = Maybe.catMaybes edges  
  return $ buildGraph (buildLinks n edges')
  where ns = [1 .. fromIntegral n]

buildLinks :: (Integral n) => n -> [(n,n)] -> [((Element,Port), (Element,Port))]
buildLinks n edges = edgeLinks ++ hostLinks 
  where switches = Set.toList . Set.fromList $ uncurry (++) $ unzip edges
        edgeLinks = map (\ (n1, n2) -> ((Switch $ fromIntegral n1, fromIntegral n2), 
                                        (Switch $ fromIntegral n2, fromIntegral n1))) 
                    edges
        hostLinks = buildHosts switches 
        buildHosts switches = concatMap addHosts switches
        addHosts switch = [ ((Switch $ fromIntegral switch, fromIntegral host), (Host $ fromIntegral host, 0))
                          | host <- [100 * switch + 1 .. 100 * switch + n]]

-- |Build a Watts-Strogatz graph on n nodes, with degree k (even, n >> k >>
-- ln(n) >> 1) and rewiring likelihood 0 <= b <= 1.  See
-- en.wikipedia.org/wiki/Watts_and_Strogatz_model for more information, and the
-- algorithm we follow.  Suggested parameters: k = n/3, b=0.3
smallworld :: (Integral n, Integral h, Integral k) => n -> h -> k -> Double -> IO Graph
smallworld n h k b = do
  edges' <- mapM rewire edges
  let edges'' = Set.toList . Set.fromList $ edges'
  return $ buildGraph (buildLinks n edges'')
  where
  ns = [1 .. fromIntegral n]
  -- Get the edges in the ring forcing i < j
  edges = [ (i, j)
          | i <- ns, j <- [i + 1 .. n] -- => i < j => abs(i - j) > 0
          , abs (i - j) <= fromIntegral k `quot` 2]
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
fattree :: Graph
fattree = buildGraph links where
  a1 = Switch 1
  a2 = Switch 2
  t1 = Switch 11
  t2 = Switch 12
  t3 = Switch 13
  t4 = Switch 14
  links = [ ((a1, 1), (t1, 1))
          , ((a2, 1), (t1, 2))
          , ((a1, 2), (t2, 1))
          , ((a2, 2), (t2, 2))
          , ((a1, 3), (t3, 1))
          , ((a2, 3), (t3, 2))
          , ((a1, 4), (t4, 1))
          , ((a2, 4), (t4, 2))
          ] ++ hosts
  hosts = [ ((Switch $ fromIntegral t, h), (Host $ fromIntegral h, 0))
          | t <- [11, 12, 13, 14], h <- [t * 10 .. t * 10 + 5]]
