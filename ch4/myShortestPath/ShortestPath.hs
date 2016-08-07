import TwoDimVecImp1 (PrimMonad', MTDMVector, constVec, read, write, toList')

type Vertex = Int
type Weight = Maybe Int

type Graph = MTDMVector Weight

graph :: Graph
graph = 
    constVec 5 6 Nothing >>= \vec ->
    write vec 1 1 (Just 10) >>
    write vec 2 2 (Just 20) >>
    return vec

shortestPaths :: [Vertex] -> Graph -> Graph
shortestPaths [] g = g
