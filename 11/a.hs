import Data.Maybe
import Data.Hashable
import Data.HashMap.Strict
import Data.List.Split
import Data.Char

lookupMust :: (Eq k, Hashable k) => k -> HashMap k v -> v
lookupMust k map = fromJust $ Data.HashMap.Strict.lookup k map

parseEntry :: String -> (String, [String])
parseEntry s = (take 3 s, wordsBy isSpace $ drop 4 s)

pathCount :: String -> HashMap String [String] -> Int
pathCount "out" _ = 1
pathCount parent children = sum $ [pathCount child children | child <- lookupMust parent children]

main :: IO ()
main = do
  lines <- readFile "11/input.txt" >>= (pure . lines)
  let children = fromList $ parseEntry <$> lines
  print $ pathCount "you" children

