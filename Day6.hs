import qualified Data.Set as S
import Data.List

main :: IO ()
main = do
    input <- readFile "6.txt"
    print $ count (take 4 input) (drop 4 input) 4
    print $ count (take 14 input) (drop 14 input) 14

count :: String -> String -> Int -> Int
count window xs n = if window == nub window then n else count (tail window ++ [head xs]) (tail xs) (n+1)