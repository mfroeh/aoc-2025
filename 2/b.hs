import Data.List.Split

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

isValid :: Int -> Bool
isValid x =
  let xs = show x
      len = length xs
   in any (>1) [patternOccurances (take x xs) xs | x <- [1 .. len]]


-- finds all occurances of a pattern in a string, provided that the string is made up only of the pattern
-- if the string is not made up only of the pattern, returns 0
-- otherwise the number of occurancse of the pattern are returned
patternOccurances :: String -> String -> Int
patternOccurances pat xs = patternOccurances' pat xs 0

patternOccurances' :: String -> String -> Int -> Int
patternOccurances' pat xs occ
  | length xs == 0 = occ
  | (length xs) `mod` (length pat) /= 0 = 0
  | take (length pat) xs /= pat = 0
  | otherwise = patternOccurances' pat (drop (length pat) xs) (occ + 1)

main :: IO ()
main = do
  contents <- readFile "2/input.txt"
  let text = dropLast contents
  let ranges = fmap (\x -> fmap read (splitOn "-" x)) (splitOn "," text) :: [[Int]]
  let result = sum $ concat $ fmap (\range -> [x | x <- [head range .. last range], isValid x]) ranges
  print result
