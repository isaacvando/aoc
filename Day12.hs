import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = (Int,Int)
type Grid = Map.Map Pos Char
type Path = [Pos]
type Visited = Set.Set Pos

main :: IO ()
main = do
    input <- readFile "input/12.txt"
    let grid = Map.fromList ((getMap . lines) input)
    let start = head (locations (== 'S') input)
    let end = head (locations (== 'E') input)
    let starts = locations (\x -> x == 'S' || x == 'a') input
    print $ (subtract 1 . length . head . search grid Set.empty [[start]]) end
    print $ (minimum . map (subtract 1 . length) . search grid (foldr Set.insert Set.empty starts) (map (:[]) starts)) end

search :: Grid -> Visited -> [[Pos]] -> Pos -> [[Pos]]
search grid vis paths end
    | (not . null) successes = paths'
    | otherwise = search grid vis' paths' end
    where 
        paths' = getPaths grid vis paths
        vis' = foldr Set.insert vis (map head paths')
        successes = filter ((== end) . head) paths'
        
getPaths :: Grid -> Visited -> [Path] -> [Path]
getPaths _ _ [] = []
getPaths grid vis (x:xs) = map (:x) moves ++ getPaths grid vis' xs
    where 
        isLegal from to = case (Map.lookup from grid, Map.lookup to grid) of
            (Just x, Just y) -> x >= pred y && not (Set.member to vis)
            _ -> False
        moves = let (r,c) = head x in filter (isLegal (r,c)) [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]
        vis' = foldr Set.insert vis moves

getMap :: [String] -> [((Int,Int),Char)]
getMap xss = concat [ [ ((row,col), adjust ((xss !! row) !! col)) | col <- [0..length (head xss) -1]] | row <- [0..length xss -1]]
    where
        adjust c = case c of
            'S' -> 'a'
            'E' -> 'z'
            x -> x

locations :: (Char -> Bool) -> String -> [Pos]
locations pred input = let xss = lines input in
    concat [[ (row,col) | col <- [0..length (head xss) -1], pred ((xss !! row) !! col)] | row <- [0..length xss -1]]
