import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.MemoCombinators qualified as Memo
import Data.Hashable
import Data.List.Split
import Data.Maybe
import System.IO.Unsafe

-- global constant
children :: HM.HashMap String [String]
children = unsafePerformIO $ do
  lines <- readFile "11/input.txt" >>= (pure . lines)
  pure $ HM.fromList $ parseEntry <$> lines
{-# NOINLINE children #-}

lookupMust :: (Eq k, Hashable k) => k -> HM.HashMap k v -> v
lookupMust k map = fromJust $ HM.lookup k map

parseEntry :: String -> (String, [String])
parseEntry s = (take 3 s, wordsBy isSpace $ drop 4 s)

pathCount :: String -> Bool -> Bool -> Int
pathCount = Memo.memo3 (Memo.list Memo.char) Memo.bool Memo.bool pathCount'
  where 
    pathCount' :: String -> Bool -> Bool -> Int
    pathCount' "out" True True = 1
    pathCount' "out" _ _ = 0
    pathCount' node fftSeen dacSeen = sum $ [pathCount child (fftSeen || node == "fft") (dacSeen || node == "dac") | child <- lookupMust node children]

main :: IO ()
main = print $ pathCount "svr" False False
