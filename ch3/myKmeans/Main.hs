import Data.Binary (decodeFile)
-- import KMeansVector4
import KMeans5
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.Exception (evaluate)
import System.Mem (performGC)

main = do
    points <- decodeFile "points.bin"
    clusters <- read `fmap` readFile "clusters"
    let nclusters = length clusters
    npoints <- evaluate (length points)
    performGC
    t0 <- getCurrentTime
    final_clusters <- kmeans_seq nclusters points clusters
    t1 <- getCurrentTime
    print final_clusters
    printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)
