parseInstruction :: String -> Int
parseInstruction ('R' : xs) = read xs
parseInstruction ('L' : xs) = -read xs

addAndTrack :: [Int] -> Int -> [Int]
addAndTrack (x : xs) y = (x + y) `mod` 100 : timesClicked x y : x : xs

timesClicked :: Int -> Int -> Int
timesClicked x y = abs (x + y) `div` 100 + if x + y <= 0 && x > 0 && y /= 0 then 1 else 0

takeEverySecond :: [Int] -> [Int]
takeEverySecond (_ : xx : xs) = xx : takeEverySecond xs
takeEverySecond [] = []

main :: IO ()
main = do
  contents <- readFile "1/input.txt"
  let insts = map parseInstruction (lines contents)
  let intermediateResults = foldl addAndTrack [50, 0] insts
  let password = foldl (+) 0 (takeEverySecond intermediateResults)
  print password
