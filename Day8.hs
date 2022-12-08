import Data.Char
import qualified Data.Map as Map

main :: IO ()
main = do
    input <- readFile "input/8.txt"
    let rows = length (lines input)
    let cols = length ((lines input) !! 0)
    let vals = (getMap . map (map digitToInt) . lines) input
    print $ (foldr (\(_,x) acc -> acc + x) 0 . Map.toList . Map.mapWithKey (f vals rows cols)) vals
        where 
            f theMap rows cols (row, col) val = if all (\x -> comp (row - x, col) theMap val) [1..rows -1] 
                    || all (\x -> comp (row + x, col) theMap val) [1..rows -1] 
                    || all (\x -> comp (row, col - x) theMap val) [1..cols -1]
                    || all (\x -> comp (row, col + x) theMap val) [1..cols -1]
                    then 1 else 0

comp :: (Int,Int) -> Map.Map (Int,Int) Int -> Int -> Bool
comp (x,y) theMap val = case Map.lookup (x,y) theMap of
                Nothing -> True
                Just x -> x < val

getMap :: [[Int]] -> Map.Map (Int, Int) Int
getMap xs = Map.fromList $ concat [[ ((row,col), (xs !! row) !! col) | col <- [0..length (xs !! 0) - 1]] | row <- [0..length xs - 1]]