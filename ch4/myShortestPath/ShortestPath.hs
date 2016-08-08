import TwoDimVecImp1 (PrimMonad', MTDMVector, constVec, read, write, toList')
import Prelude hiding (min, read)

type Vertex = Int
type Weight = Maybe Int

type Graph = MTDMVector Weight

graph :: Graph
graph = 
    constVec 5 6 Nothing >>= \vec ->
    write vec 1 1 (Just 10) >>
    write vec 2 2 (Just 20) >>
    return vec

add :: Weight -> Weight -> Weight
(Just a) `add` (Just b) = Just (a + b)
_ `add` _ = Nothing

smaller :: Weight -> Weight -> Bool
(Just a) `smaller` (Just b) = a < b
(Just a) `smaller` Nothing = True
Nothing `smaller` _ = False

min :: Weight -> Weight -> Weight
min a b
    | a `smaller` b = a
    | otherwise = b

shortestPath :: Vertex -> Vertex -> Vertex -> Graph -> Graph
shortestPath i j 0 graph = graph
shortestPath i j k graph = 
        graph >>= \graph' ->
        write graph' i j res >>
        return graph'
    where 
        ij = graph >>= \graph' -> read graph' i j
        ik = graph >>= \graph' -> read graph' i k
        kj = read graph' k j
        res = ij `min` (ik + kj)
