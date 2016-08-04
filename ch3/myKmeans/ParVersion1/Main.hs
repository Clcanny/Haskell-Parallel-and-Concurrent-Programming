import Data.Binary (decodeFile)
import KMeans
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.Exception (evaluate)
import System.Mem (performGC)
import System.Environment

main = do
    points <- decodeFile "points.bin"
    clusters <- read `fmap` readFile "clusters"
    let nclusters = length clusters
    npoints <- evaluate (length points)
    args <- getArgs
    performGC
    t0 <- getCurrentTime
    final_clusters <- kmeans_strat (read $ head args) nclusters points clusters
    t1 <- getCurrentTime
    print final_clusters
    printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)
