import Data.List
import Data.Maybe

splitBeam :: Int -> [Int] -> [Int]
splitBeam i ss = if isJust $ find (==i) ss then [i-1, i+1] else [i]

splitBeams :: Int -> [Int] -> [Int] -> [Int]
splitBeams u bs ss = filter (\x -> 0 <= x && x <= u) $ nub $ concat [splitBeam b ss | b <- bs]

countSplits :: [Int] -> [Int] -> Int
countSplits bs ss = length $ filter (\x -> (length x) > 1) [splitBeam b ss | b <- bs]

simulate :: Int -> [Int] -> [[Int]] -> Int
simulate _ _ [] = 0
simulate u bs (ss:splitters) = (countSplits bs ss) + simulate u (splitBeams u bs ss) splitters

main :: IO ()
main = do
  lines <- lines <$> (readFile "7/input.txt")
  let splitters = findIndices (=='^') <$> (drop 1 lines)
  let start = fromJust $ findIndex (=='S') $ head lines
  let upper = (length $ head lines) - 1
  let res = simulate upper [start] splitters
  print res
