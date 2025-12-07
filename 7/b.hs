import Data.List
import Data.Maybe
import Data.MemoCombinators qualified as Memo
import System.IO.Unsafe

-- global constant
splitters :: [[Int]]
splitters = unsafePerformIO $ do
    contents <- readFile "7/input.txt"
    let ls = drop 1 (lines contents)
    return $ map (findIndices (== '^')) ls
{-# NOINLINE splitters #-}

timelines :: Int -> Int -> Int
timelines = Memo.memo2 Memo.integral Memo.integral timelines'
  where
    timelines' :: Int -> Int  -> Int
    timelines' b off
      | off >= (length splitters) = 1
      | isJust $ find (==b) (splitters!!off) = timelines (b - 1) (off+1) + timelines (b + 1) (off+1)
      | otherwise = timelines b (off+1)

main :: IO ()
main = do
  lines <- lines <$> (readFile "7/input.txt")
  let start = fromJust $ findIndex (== 'S') $ head lines
  let res = timelines start 0
  print res
