import Data.List.Split
import Data.Char

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs


transpose :: [[a]] -> [[a]]
transpose mat = [[ mat!!y!!x |  y <- [0..(length mat - 1)]] | x <- [0..(length $ head mat) - 1]]

apply :: String -> [Int] -> Int
apply "*" xs = foldl (*) 1 xs
apply "+" xs = foldl (+) 0 xs

main :: IO ()
main = do
  lines <- lines <$> (readFile "6/input.txt")
  let nums = transpose $ (fmap . fmap) read $ wordsBy isSpace <$> (dropLast lines) :: [[Int]]
  let ops = wordsBy isSpace (last lines) :: [String]
  let res = sum $ zipWith apply ops nums
  print res
