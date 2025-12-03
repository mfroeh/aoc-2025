maximumWithIndex :: (Ord a) => [a] -> (Int, a)
maximumWithIndex xs = maximumWithIndex' xs 0 (0, head xs)

maximumWithIndex' :: (Ord a) => [a] -> Int -> (Int, a) -> (Int, a)
maximumWithIndex' [] _ cur = cur
maximumWithIndex' (x : xs) idx (cidx, cele) =
  if x > cele
    then maximumWithIndex' xs (idx + 1) (idx, x)
    else maximumWithIndex' xs (idx + 1) (cidx, cele)

slice :: [a] -> Int -> Int -> [a]
slice xs from to = take (to - from) (drop from xs)

maximumInSlice :: (Ord a) => [a] -> Int -> Int -> (Int, a)
maximumInSlice xs from to =  addOffset' (maximumWithIndex (slice xs from to)) from

addOffset' :: (Int, a) -> Int -> (Int, a)
addOffset' (idx, x) off = (idx + off, x)

findMaximumNumber :: String -> String
findMaximumNumber xs = tail $ fmap snd $ foldl (\res i -> res ++ [maximumInSlice xs ((fst $ last res) + 1) (length xs - (12 - i))]) [(-1, '0')] [1 .. 12]

main :: IO ()
main = do
  contents <- readFile "3/input.txt"
  let banks = lines contents
  let maxes = sum $ fmap (read . findMaximumNumber) banks
  print maxes
