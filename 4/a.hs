import Data.Maybe

type Grid = [[Char]]

at :: Grid -> (Int, Int) -> Maybe Char
at grid (row, col)
  | 0 <= row && row < (length grid) && 0 <= col && col < (length $ head grid) = Just (grid !! row !! col)
  | otherwise = Nothing

value :: Char -> Int
value '@' = 1
value _ = 0

valueAt :: Grid -> (Int, Int) -> Int
valueAt grid (row, col)
  | isNothing (grid `at` (row, col)) = 0
  | otherwise = value $ fromJust (grid `at` (row, col))

chunkValue :: Grid -> (Int, Int) -> Maybe Int
chunkValue grid (row, col)
  | grid `at` (row, col) /= Just '@' = Nothing
  | otherwise = Just $ sum [valueAt grid (row + roff, col + coloff) | roff <- [-1, 0, 1], coloff <- [-1, 0, 1]] - 1

main :: IO ()
main = do
  grid <- fmap lines (readFile "4/input.txt")
  let chunkValues = [chunkValue grid (row, col) | row <- [0 .. (length grid) - 1], col <- [0 .. (length $ head grid) - 1]]
  let count = length $ filter isJust $ filter (< Just 4) chunkValues
  print count
