module Day13 where

import Data.List.Split

main :: IO ()
main = do
    input <- readFile "foo.txt"
    let pairs = (splitOn [""] . lines) input
    print pairs

compare :: String -> String -> Bool
compare p1 p2 = False

peel :: String -> String
peel xs@('[':y:_) = (init . tail) xs
peel x = x
