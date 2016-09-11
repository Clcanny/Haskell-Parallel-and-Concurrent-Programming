{-# OPTIONS_GHC -Wall #-}
import System.Directory
import System.FilePath
import System.Environment
import Data.List hiding (find)

-- <<main
main :: IO ()
main = do
  [s,d] <- getArgs
  r <- find s d
  print r
-- >>

-- <<find
find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  -- getDirectoryContents :: FilePath -> IO [FilePath]
  fs <- getDirectoryContents d
  -- filter :: (a -> Bool) -> [a] -> [a]
  -- 此处假设文件夹和文件名是不可以相同的，即同一文件夹下，只能有一个某名字的文件或子文件夹
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
     then return (Just (d </> s))
     else loop fs'
 where
  loop [] = return Nothing
  loop (f:fs) = do
    let d' = d </> f
    isdir <- doesDirectoryExist d'
    if isdir
       then do r <- find s d'
               case r of
                 Just _  -> return r
                 Nothing -> loop fs
       else loop fs
-- >>

-- test method
-- runhaskell findseq.hs findseq.hs ./
-- runhaskell findseq.hs cantfind.hs ./
