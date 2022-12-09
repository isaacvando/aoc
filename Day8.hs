import Data.Char
import qualified Data.Map as Map
import Data.Bifunctor

type Pos = (Int,Int)

main :: IO ()
main = do
    input <- readFile "input/8.txt"
    let trees = (getPositions . map (map digitToInt) . lines) input
    let treeMap = Map.fromList trees
    print $ (length . filter id . map or . map (\(k, v) -> map (comp treeMap k v) moves)) trees
    print $ (maximum . map product . map (\(k, v) -> map (count treeMap k v) moves)) trees
        where 
            moves :: [Pos -> Pos]
            moves = [first (+1), first (+(-1)), second (+1), second (+(-1))]

comp :: Map.Map Pos Int -> Pos -> Int -> (Pos -> Pos) -> Bool
comp trees position val move = let pos = move position in case Map.lookup pos trees of
    Nothing -> True
    Just x -> (x < val) && comp trees pos val move

count :: Map.Map Pos Int -> Pos -> Int -> (Pos -> Pos) -> Int
count trees position val move = let pos = move position in case Map.lookup pos trees of
    Nothing -> 0
    Just x -> if x >= val then 1 else 1 + count trees pos val move

getPositions :: [[Int]] -> [(Pos,Int)]
getPositions xs = concat [[ ((row,col), (xs !! row) !! col) | col <- [0..length (head xs) - 1]] | row <- [0..length xs - 1]]
