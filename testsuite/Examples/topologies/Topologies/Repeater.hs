{-# LANGUAGE
    TypeSynonymInstances
 #-}

module Topologies.Repeater where

import qualified Topologies as TS

import Data.Bits
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Frenetic.Compat (Port)

-- Topology for a single repeater switch connecting two hosts:
--      h1 -- switch -- h2
type Topology = (Gr TS.NodeLabel TS.EdgeLabel)
instance TS.Topology Topology where
    hosts g = map fst $ filter f $ labNodes g
        where f (_, ntype) = case ntype of
                                 TS.Host -> True
                                 _ -> False

    switches g = map fst $ filter f $ labNodes g
        where f (_, ntype) = case ntype of
                                 TS.Switch -> True
                                 _ -> False

    links g = edges g

    port g n1 n2 = foldl f Nothing $ lsuc' $ context g n1
        where f Nothing (n, port) | n == n2 = Just port
                                  | otherwise = Nothing
              f (Just p) _ = Just p

    -- ip g n = 10.0.0.0 .|. n
    ip g n = 167772160 .|. fromIntegral n

    mkTopo numHosts numSwitches = mkGraph lNodes lEdges
        where lNodes = map (\n -> (n, TS.Host)) hosts ++ map (\n -> (n, TS.Switch)) switches
              hosts = [1, 2]
              switches = [101]
              lEdges = [(1,101,2), (101,1,1), (2,101,1), (101,2,1)]


