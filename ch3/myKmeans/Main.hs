import Data.Binary (decodeFile)
import KMeansSeq
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.Exception (evaluate)

main = do
    points <- decodeFile "points.bin"
    clusters <- read `fmap` readFile "clusters"
    let nclusters = length clusters
    printf "nclusters: %d\n" nclusters
    npoints <- evaluate (length points)
    printf "npoints: %d\n" npoints
    t0 <- getCurrentTime
    final_clusters <- kmeans_seq nclusters points clusters
    t1 <- getCurrentTime
    print final_clusters
    printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)
