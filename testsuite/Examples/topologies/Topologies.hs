{-# LANGUAGE
    TypeSynonymInstances
    , FlexibleInstances
 #-}

module Topologies where

import Data.Bits
import Data.Word
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Text.Parsec
import Text.Parsec.Prim
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Frenetic.Compat (Port)

data NodeLabel = Host | Switch
    deriving (Eq, Show)
type EdgeLabel = Port

{- A Topology is a directed graph with Host/Switch labels on nodes and Port
labels on edges. -}
type Topology = Gr NodeLabel EdgeLabel

instance Show Topology where
    show t =
      foldl (\s (e1,e2) -> s ++ "\n" ++ show e1 ++ " <-> " ++ show e2) "" $ links t

hosts :: Topology -> [Node]
hosts g = map fst $ filter f $ labNodes g
    where f (_, ntype) = case ntype of
                             Host -> True
                             _ -> False

switches :: Topology -> [Node]
switches g = map fst $ filter f $ labNodes g
    where f (_, ntype) = case ntype of
                             Switch -> True
                             _ -> False

port :: Topology -> Node -> Node -> Maybe Port

links :: Topology -> [Edge]
links g = edges g

port g n1 n2 = foldl f Nothing $ lsuc' $ context g n1
    where f Nothing (n, port) | n == n2 = Just port
                              | otherwise = Nothing
          f (Just p) _ = Just p

ip :: Topology -> Node -> Word32
-- ip g n = 10.0.0.0 .|. n
ip g n = 167772160 .|. fromIntegral n

parseTopo :: String -> Topology
parseTopo input = mkGraph nodes edges
  where
    (nodes, edges) = parseTopology input

-- Parse a textual representation of a topology and return a pair
-- of lists representing the nodes and edges.
--
-- Each line has the following format:
--
-- node L: (n1, p1), (n2, p2), ..., (nn, pn)
--
-- where this denotes labeled edges (node, neighbor1, port1), ...,
-- and a labeled node (node, L).
--
-- More precisely,
--
-- line         ::= nodeLabel node ':' neighborList
-- neighborList     ::= neighbor ',' neighborList | ''
-- neighbor         ::= '(' node ',' edgeLabel ')'
-- node         ::= digit+
-- edgeLabel    ::= digit+
-- nodeLabel    ::= 'Host' | 'Switch'
-- digit        ::= '0 | '1' | ... | '9'
--

parseTopology :: String -> ([LNode NodeLabel], [LEdge EdgeLabel])
parseTopology input = case parse parser "" input of
  Left err -> error $ show err
  Right v -> v
  where
    parser :: Parsec String () ([LNode NodeLabel], [LEdge EdgeLabel])
    parser = do{ skipMany newline
               ; lines <- line `sepBy1` (skipMany newline)
               ; let (nodes, edgeMultiset) = unzip lines
               ; return (nodes, foldl (\s1 s2 -> s1 ++ s2) [] edgeMultiset)
               }
    line :: Parsec String () (LNode NodeLabel, [LEdge EdgeLabel])
    line = do{ nodeLabel <- choice [host, switch]
             ; node      <- natural
             ; colon
             ; neighbors     <- neighbor `sepBy` comma
             ; return ((fromIntegral node, nodeLabel), map (\(n, p) -> (fromIntegral node, n, p)) neighbors)
             }
    neighbor :: Parsec String () (Node, Port)
    neighbor = parens neighbor'
    neighbor' = do { n <- natural
               ; comma
               ; p <- natural
               ; return (fromIntegral n, fromIntegral p)
               }
    host = do{ reserved "Host" ; return Host }
    switch = do{ reserved "Switch" ; return Switch }

topologyDef :: P.LanguageDef st
topologyDef = haskellDef { P.reservedNames = ["Host", "Switch"]
                         , P.reservedOpNames = []
                         }
lexer :: P.TokenParser ()
lexer = P.makeTokenParser topologyDef
reserved = P.reserved lexer
natural = P.natural lexer
parens = P.parens lexer
colon = P.colon lexer
comma = P.comma lexer

