import Data.Char
import qualified Data.Map as Map

type Pos = (Int,Int)

main :: IO ()
main = do
    input <- readFile "input/8.txt"
    let rowCount = length (lines input)
    let columnCount = length ((lines input) !! 0)
    let trees = (getMap . map (map digitToInt) . lines) input
    print $ (foldr (\(_,x) acc -> acc + x) 0 . Map.toList . Map.mapWithKey (f trees rowCount columnCount)) trees
    print $ (maximum . map product . map snd . Map.toList . Map.mapWithKey (f' trees rowCount columnCount)) trees
        where 
            f trees rowCount colCount (row, col) val = if all (\x -> comp (row - x, col) trees val) [1..rowCount] 
                    || all (\x -> comp (row + x, col) trees val) [1..rowCount] 
                    || all (\x -> comp (row, col - x) trees val) [1..colCount]
                    || all (\x -> comp (row, col + x) trees val) [1..colCount]
                    then 1 else 0
            f' trees rowCount colCount position val = 
                map (count position trees val) [\(x,y) -> (x-1, y), \(x,y) -> (x+1, y), \(x,y) -> (x, y-1), \(x,y) -> (x, y+1)]

comp :: Pos -> Map.Map Pos Int -> Int -> Bool
comp (x,y) trees val = case Map.lookup (x,y) trees of
    Nothing -> True
    Just x -> x < val

count :: Pos -> Map.Map Pos Int -> Int -> (Pos -> Pos) -> Int
count position trees val getPos = let pos = getPos position in case Map.lookup (getPos position) trees of
    Nothing -> 0
    Just x -> if x >= val then 1 else 1 + count pos trees val getPos

getMap :: [[Int]] -> Map.Map Pos Int
getMap xs = Map.fromList $ concat [[ ((row,col), (xs !! row) !! col) | col <- [0..length (xs !! 0) - 1]] | row <- [0..length xs - 1]]
