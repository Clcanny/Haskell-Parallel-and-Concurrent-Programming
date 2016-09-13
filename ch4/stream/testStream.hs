import qualified Stream as S
import qualified Stream2 as S2

list = [1..500] :: [Int]

res = S.runPar $
  S.streamFromList list >>= \stream ->
  S.streamMap (+1) stream >>= \stream' ->
  S.streamMap (*2) stream' >>= \stream'' ->
  S.streamFold (+) 0 stream''

res' = S2.runPar $
  S2.streamFromList 5 5 list >>= \stream ->
  S2.streamMap (+1) stream >>= \stream' ->
  S2.streamMap (*2) stream' >>= \stream'' ->
  S2.streamFold (+) 0 stream''

main = print $ res == res'
