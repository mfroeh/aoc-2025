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

newCell :: Grid -> (Int, Int) -> Char
newCell grid pos =
  if (isJust $ chunkValue grid pos) && (chunkValue grid pos < (Just 4))
    then 'x'
    else fromJust $ grid `at` pos

updateGrid :: Grid -> Grid
updateGrid grid = [[newCell grid (row, col) | col <- [0 .. (length $ head grid) - 1]] | row <- [0 .. (length grid) - 1]]

gridCount :: Grid -> Int
gridCount grid = length $ filter (== '@') (concat grid)

gridDiff :: Grid -> Grid -> Int
gridDiff old new = gridCount old - gridCount new

exhaustiveUpdates :: Grid -> Grid -> Int -> Int
exhaustiveUpdates old new cur
  | gridDiff old new == 0 = cur
  | otherwise = gridDiff old new + exhaustiveUpdates new (updateGrid new) cur

main :: IO ()
main = do
  grid <- fmap lines (readFile "4/input.txt")
  print $ exhaustiveUpdates grid (updateGrid grid) 0
