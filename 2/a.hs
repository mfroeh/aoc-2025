import Data.List.Split

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

isValid :: Int -> Bool
isValid x =
  let xs = show x
      len = length xs
   in if odd len
        then False
        else isValid' (take (len `div` 2) xs) (drop (len `div` 2) xs)

isValid' :: String -> String -> Bool
isValid' [] [] = True
isValid' (x : xs) (y : ys) = if x == y then isValid' xs ys else False

main :: IO ()
main = do
  contents <- readFile "2/input.txt"
  let text = dropLast contents
  let ranges = fmap (\x -> fmap read (splitOn "-" x)) (splitOn "," text) :: [[Int]]
  let result = sum $ concat $ fmap (\range -> [x | x <- [head range .. last range], isValid x]) ranges
  print result
