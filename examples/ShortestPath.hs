module ShortestPath
       where

import Data.Array

data Edge = Edge {start :: Node,
                  end :: Node,
                  port :: Int}

data Node = Node {edges :: [Edge],
                  ip :: Int,
                  mac :: Int}

type Graph = [Node]

replaceNth n f (x:xs)
  | n == 0 = (f x):xs
  | otherwise = x:replaceNth (n-1) f xs
replaceNth n newVal [] =
  fail "Trying to replace a value that is out of range"

updatePath :: [[Maybe Int]] -> Int -> Int -> Maybe Int -> [[Maybe Int]]
updatePath path ind1 ind2 val =
  replaceNth ind1 (\x -> replaceNth ind2 (\y -> val) x) path

getVal :: [[Maybe Int]] -> Int -> Int -> Maybe Int
getVal path ind1 ind2 = 
  ((path !! ind1) !! ind2)
  
calcBest :: [[Maybe Int]] -> Int -> Int -> Int -> Maybe Int
calcBest path k i j =
  let inter1 = getVal path i k
      inter2 = getVal path k j
      direct = getVal path i j in
  let combine =
        case (inter1, inter2) of
          (Just a, Just b) -> Just (a + b)
          _ -> Nothing in
  case (direct, combine) of
    (Just a, Just b) -> Just (min a b)
    (Just a, Nothing) -> Just a
    (Nothing, Just b) -> Just b
    (Nothing, Nothing) -> Nothing
      
generatePaths :: Int -> Int -> [[Maybe Int]]
generatePaths i j =
  foldr (\x ac-> (foldr (\y acc-> Nothing:acc) [] [1..j]):ac)  [] [1..i]

floydMarshall :: Graph -> Int
floydMarshall graph= 
  let n = length graph in
  let outerLoop path k =
        middleLoop path k 0
      middleLoop path k i =
        innerLoop path k i 0
      innerLoop path k i j =
        updatePath path i j (calcBest path k i j)
        in
  1