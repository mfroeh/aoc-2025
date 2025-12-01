parseInstruction :: String -> Int
parseInstruction ('R' : xs) = read xs
parseInstruction ('L' : xs) = -read xs

addAndTrack :: [Int] -> Int -> [Int]
addAndTrack (x : xs) y = (mod (x + y) 100) : x : xs

main :: IO ()
main = do
  contents <- readFile "1/input.txt"
  let insts = map parseInstruction (lines contents)
  let intermediateResults = foldl addAndTrack [50] insts
  let password = length (filter (== 0) intermediateResults)
  print password
