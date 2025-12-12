import Data.Char
import Data.List.Split
import Data.Maybe
import Text.Regex

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

parseGoal :: String -> [Int]
parseGoal [] = []
parseGoal (x : xs) = if x == '.' then 0 : parseGoal xs else 1 : parseGoal xs

parseAction :: Int -> String -> [Int]
parseAction len str =
  let idxs = read <$> wordsBy (== ',') str :: [Int]
   in [fromEnum $ elem x idxs | x <- [0 .. len - 1]]

parseGame :: String -> ([Int], [[Int]])
parseGame str =
  let re = mkRegex "\\[(.+)\\](.+)\\{.+"
      matches = fromJust $ matchRegex re str
      goal = parseGoal $ matches !! 0
      buttons = (parseAction (length goal) . drop 1 . dropLast) <$> wordsBy isSpace (matches !! 1)
   in (goal, buttons)

applyAction :: [Int] -> [Int] -> [Int]
applyAction state a = (`mod` 2) <$> zipWith (+) state a

solve :: [Int] -> [Int] -> [[Int]] -> Int -> Int
solve goal state [] ac = if state == goal then ac else 100000
solve goal state (b : bs) ac =
  if state == goal
    then ac
    else min (solve goal state bs ac) (solve goal (applyAction state b) bs (ac + 1))

main :: IO ()
main = do
  games <- readFile "10/input.txt" >>= (pure . (fmap parseGame) . lines)
  let solved = [solve goal (replicate (length goal) 0) actions 0 | (goal, actions) <- games]
  print $ sum $ solved
