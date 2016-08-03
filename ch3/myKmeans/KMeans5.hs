module KMeans5 where

import Data.List (minimumBy)
import Control.DeepSeq (NFData, rnf)
import Data.Binary (Binary)
import qualified Data.Binary as Bi
import Data.Function (on)
import Control.Monad.State (State, get, put, runState)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector as Vector
import Data.Vector (Vector)

data Point = Point {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Show, Read, Eq)

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
    clId :: {-# UNPACK #-} !Int,
    clCent :: {-# UNPACK #-} !Point
} deriving (Show, Read, Eq)

data PointSum = PointSum {-# UNPACK #-} !Int {-# UNPACK #-} !Double {-# UNPACK #-} !Double

getPointNum :: PointSum -> Int
getPointNum (PointSum count _ _) = count

zeroPointSum :: PointSum
zeroPointSum = PointSum 0 0 0

addToPointSum :: Point -> PointSum -> PointSum
addToPointSum (Point x y) (PointSum count xs ys) = PointSum (count + 1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) = Cluster i $ Point (xs / fromIntegral count) (ys / fromIntegral count)

assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
        vec <- MVector.replicate nclusters (zeroPointSum)
        let addpoint p = do
                let c = nearest p; cid = clId c
                ps <- MVector.read vec cid
                MVector.write vec cid $! addToPointSum p ps
        mapM_ addpoint points
        return vec
    where
        nearest p = fst $ minimumBy (compare `on` snd) [ (c, sqDistance (clCent c) p) | c <- clusters ]

makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vec =
    [ pointSumToCluster i ps
    | (i, ps@(PointSum count _ _)) <- zip [0..] (Vector.toList vec)
    , count > 0
    ]

step :: Int -> [Cluster] -> [Point] -> [Cluster]
step nclusters clusters points = makeNewClusters (assign nclusters clusters points)

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
