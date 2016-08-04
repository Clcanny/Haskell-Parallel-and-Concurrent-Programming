module KMeansList where

import Data.List (minimumBy)
import Control.DeepSeq (NFData, rnf)
import Data.Binary (Binary)
import qualified Data.Binary as Bi
import Data.Function (on)
import Control.Monad.State (State, get, put, runState)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as B

data Point = Point !Double !Double deriving (Show, Read, Eq)

instance NFData Point where
    rnf (Point x y) = () -- all fields are strict

instance Binary Point where
    put (Point a b) = Bi.put a >> Bi.put b
    get = do a <- Bi.get; b <- Bi.get; return (Point a b)

readPoints :: FilePath -> IO [Point]
readPoints f = do
    s <- B.readFile f
    let ls = map B.words $ B.lines s
        points = [ Point (read (B.unpack sx)) (read (B.unpack sy))
                 | (sx:sy:_) <- ls ]
    return points

zeroPoint :: Point
zeroPoint = Point 0 0

sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

data Cluster = Cluster {
    clId :: Int,
    clCent :: Point
} deriving (Show, Read, Eq)

data PointSum = PointSum !Int !Double !Double

getPointNum :: PointSum -> Int
getPointNum (PointSum count _ _) = count

zeroPointSum :: PointSum
zeroPointSum = PointSum 0 0 0

addToPointSum :: Point -> PointSum -> PointSum
addToPointSum (Point x y) (PointSum count xs ys) = PointSum (count + 1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) = Cluster i $ Point (xs / fromIntegral count) (ys / fromIntegral count)

at :: [a] -> Int -> Maybe a
at (x : xs) n
    | n > 0 = at xs (n - 1)
    | n == 0 = Just x
    | n < 0 = Nothing
at [] _ = Nothing

replaceAt :: [a] -> Int -> a -> Maybe [a]
replaceAt (x : xs) n y
    | n > 0 = 
        replaceAt xs (n - 1) y >>= \ys -> Just (x : ys)
    | n == 0 = Just (y : xs)
    | n < 0 = Nothing
replaceAt [] _ _ = Nothing

fAt :: [a] -> Int -> (a -> a) -> Maybe [a]
fAt (x : xs) n f
    | n > 0 = 
        fAt xs (n - 1) f >>= \ys -> Just (x : ys)
    | n == 0 = Just (f x : xs)
    | n < 0 = Nothing
fAt [] _ _ = Nothing

nearest :: [Cluster] -> Point -> Cluster
nearest clusters p = fst $ minimumBy (compare `on` snd)
    [ (c, sqDistance (clCent c) p) | c <- clusters ]

addpoint :: Point -> [Cluster] -> [PointSum] -> [PointSum]
addpoint p cs ps = res
    where c = nearest cs p
          (Just res) = fAt ps (clId c) (addToPointSum p)

assign :: Int -> [Cluster] -> [Point] -> State [PointSum] ()
assign nclusters clusters (p : ps) = 
    get >>= \psum ->
    put (addpoint p clusters psum) >> 
    assign nclusters clusters ps
assign nclusters clusters [] = get >>= \psum -> put psum

assign' :: Int -> [Cluster] -> [Point] -> [PointSum]
assign' nclusters clusters ps = snd $ runState (assign nclusters clusters ps) (take nclusters $ cycle [zeroPointSum]) 

makeNewClusters :: Int -> [PointSum] -> [Cluster]
makeNewClusters _ [] = []
makeNewClusters n (p : ps)
    | getPointNum p > 0 = pointSumToCluster n p : makeNewClusters (n + 1) ps
    | otherwise = makeNewClusters (n + 1) ps

makeNewClusters' :: [PointSum] -> [Cluster]
makeNewClusters' = makeNewClusters 0

step :: Int -> [Cluster] -> [Point] -> [Cluster]
step nclusters clusters points = makeNewClusters' (assign' nclusters clusters points)

kmeans_seq :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_seq nclusters points clusters =
    let
        loop :: Int -> [Cluster] -> IO [Cluster]
        loop n clusters | n > tooMany = do
            putStrLn "giving up."
            return clusters
        loop n clusters = do
            printf "iteration %d\n" n
            putStr (unlines (map show clusters))
            let clusters' = step nclusters clusters points
            if clusters' == clusters
                then return clusters
                else loop (n + 1) clusters'
    in
        loop 0 clusters

tooMany = 80
