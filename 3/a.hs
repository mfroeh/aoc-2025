maximumWithIndex :: Ord a => [a] -> (Int, a)
maximumWithIndex xs = maximumWithIndex' xs 0 (0, head xs)

maximumWithIndex' :: Ord a => [a] -> Int -> (Int, a) -> (Int, a)
maximumWithIndex' [] _ cur = cur
maximumWithIndex' (x:xs) idx (cidx, cele) = if x > cele 
                                            then maximumWithIndex' xs (idx + 1) (idx, x)
                                            else maximumWithIndex' xs (idx + 1) (cidx, cele)

main :: IO ()
main = do
  contents <- readFile "3/input.txt"
  let banks = lines contents
  let maxes = fmap (\bank -> maximumWithIndex (init bank)) banks
  let maxesAfterIdx = fmap (\(bank, (maxidx, _)) -> maximum (drop (maxidx + 1) bank)) (zip banks maxes)
  let result =  sum [read [x, y] :: Int | ((_, x), y) <- zip maxes maxesAfterIdx]
  print result
