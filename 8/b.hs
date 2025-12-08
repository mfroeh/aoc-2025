import Data.List
import Data.List.Split
import Data.Maybe

type Coordinate = (Int, Int, Int)

x :: Coordinate -> Int
x (x, _, _) = x

dist :: Coordinate -> Coordinate -> Double
dist (a1, a2, a3) (b1, b2, b3) = sqrt $ fromIntegral ((a1 - b1) ^ 2 + (a2 - b2) ^ 2 + (a3 - b3) ^ 2)

parseCoordinate :: String -> Coordinate
parseCoordinate str =
  let s = read <$> splitOn "," str :: [Int]
   in (s !! 0, s !! 1, s !! 2)

findContainingSet :: Coordinate -> [[Coordinate]] -> [Coordinate]
findContainingSet c (x : xs) = if isJust $ find (== c) x then x else findContainingSet c xs

mergeSets :: (Coordinate, Coordinate) -> [[Coordinate]] -> [[Coordinate]]
mergeSets (p, q) sets =
  let pSet = findContainingSet p sets
      qSet = findContainingSet q sets
   in if pSet == qSet
        then sets
        else [set | set <- sets, set /= pSet && set /= qSet] ++ [pSet ++ qSet]

mergeSets' :: [(Coordinate, Coordinate)] -> [[Coordinate]] -> ([[Coordinate]], (Coordinate, Coordinate))
mergeSets' (x : xs) sets = let merged = mergeSets x sets in if (length merged) == 1 then (merged, x) else mergeSets' xs merged

main :: IO ()
main = do
  coordinates <- (readFile "8/input.txt") >>= (pure . (fmap parseCoordinate) . lines)
  let pairs = [(coordinates !! i, coordinates !! j) | i <- [0 .. length coordinates - 1], j <- [(i + 1) .. length coordinates - 1]]
  let ordered = sortBy (\(p1, q1) (p2, q2) -> (dist p1 q1) `compare` (dist p2 q2)) pairs
  let sets = [[c] | c <- coordinates]
  let (p, q) = snd $ mergeSets' ordered sets
  let res = (x p) * (x q)
  print res
