import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bifunctor

type Pos = (Int,Int)
type Grid = Map.Map Pos Char
type Path = [Pos]


main :: IO ()
main = do
    input <- readFile "input/12.txt"
    let grid = Map.fromList ((getMap . lines) input)
    let (start, end) = getStartAndEnd (lines input)
    let starts = getstarts (lines input)
    print $ (subtract 1 . length . head . search grid Set.empty [[start]]) end
    print $ (minimum . map (subtract 1 . length) . search grid (foldr Set.insert Set.empty starts) (map (:[]) starts)) end

search :: Grid -> Set.Set Pos -> [[Pos]] -> Pos -> [[Pos]]
search grid visited paths end
    | (not . null) successes = paths'
    | otherwise = search grid visited' paths' end
    where 
        paths' = getPaths grid visited paths
        visited' = foldr Set.insert visited (map head paths')
        successes = filter isWin paths'
        isWin (p:ps) = p == end
        
getPaths :: Grid -> Set.Set Pos -> [Path] -> [Path]
getPaths _ _ [] = []
getPaths grid visited (x:xs) = map (:x) moves ++ getPaths grid visited' xs
    where 
        moves = getMoves grid visited x
        visited' = foldr Set.insert visited moves

getMoves :: Grid -> Set.Set Pos -> Path -> [Pos]
getMoves grid visited (pos:tail) = filter (isLegal grid visited pos) (map ($ pos) [first (+1), first (+ (-1)), second (+1), second (+ (-1))])

isLegal :: Grid -> Set.Set Pos -> Pos -> Pos -> Bool
isLegal grid visited from to = case (Map.lookup from grid, Map.lookup to grid) of
    (Just x, Just y) -> x >= pred y && not (Set.member to visited)
    _ -> False

getMap :: [String] -> [((Int,Int),Char)]
getMap xss = concat [ [ ((row,col), adjust ((xss !! row) !! col)) | col <- [0..length (head xss) -1]] | row <- [0..length xss -1]]
    where
        adjust c = case c of
            'S' -> 'a'
            'E' -> 'z'
            x -> x

getStartAndEnd :: [String] -> (Pos,Pos)
getStartAndEnd xss = (start, end)
    where 
        start = (head . concat) [ [ (row,col) | col <- [0..length (head xss) -1], (xss !! row) !! col == 'S'] | row <- [0..length xss -1]]
        end = (head . concat) [[ (row,col) | col <- [0..length (head xss) -1], (xss !! row) !! col == 'E'] | row <- [0..length xss -1]]

getstarts :: [String] -> [Pos]
getstarts xss = concat [[ (row,col) | col <- [0..length (head xss) -1], let spot = (xss !! row) !! col, spot == 'a' || spot == 'S'] | row <- [0..length xss -1]]
