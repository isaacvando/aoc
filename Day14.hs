import Data.List.Split 
import qualified Data.Map as Map
import Control.Arrow
import Data.Maybe

type Pos = (Int,Int)
type Grid = Map.Map Pos Bool

main :: IO ()
main = do
    input <- readFile "input/14.txt"
    let pairs = (map getPairs . lines) input
    let (minX,minY) = ((minimum . map fst . concat) &&& (minimum . (0:) . map snd . concat)) pairs
    let (maxX,maxY) = ((maximum . (500:) . map fst . concat) &&& (maximum . map snd . concat)) pairs
    let emptyGrid = Map.fromList $ concat [[((x,y),False) | y <- [minY..maxY]] | x <- [minX..maxX]]
    let grid = foldr putLines emptyGrid pairs
    print $ dropSand grid 0
    print $ dropSand' grid 0 (maxY+2)

dropSand :: Grid -> Int -> Int
dropSand grid n = case put grid (500,0) of
    Nothing -> n
    Just g -> dropSand g (n+1)

dropSand' :: Grid -> Int -> Int -> Int
dropSand' grid n floorPos = case put' grid (500,0) floorPos of
    Just g -> if Map.lookup (500,0) g == Just True then n+1 else dropSand' g (n+1) floorPos

put' :: Grid -> Pos -> Int-> Maybe Grid
put' grid (x,y) floorPos
    | y == floorPos-1 = Just $ Map.insert (x,y) True grid
    | notBlocked (x,y+1) = put' grid (x,y+1) floorPos
    | notBlocked (x-1,y+1) = put' grid (x-1,y+1) floorPos
    | notBlocked (x+1,y+1) = put' grid (x+1,y+1) floorPos
    | otherwise = Just $ Map.insert (x,y) True grid
    where 
        notBlocked pos = case Map.lookup pos grid of
            Nothing -> True
            Just x -> not x

put :: Grid -> Pos -> Maybe Grid
put grid (x,y) 
    | Map.lookup (x,y) grid == Nothing = Nothing
    | notBlocked (x,y+1) = put grid (x,y+1)
    | notBlocked (x-1,y+1) = put grid (x-1,y+1)
    | notBlocked (x+1,y+1) = put grid (x+1,y+1)
    | otherwise = Just $ Map.adjust (const True) (x,y) grid
    where 
        notBlocked pos = case Map.lookup pos grid of
            Nothing -> True
            Just x -> not x

putLines :: [(Int,Int)] -> Grid -> Grid
putLines [x] grid = grid
putLines ((x,y):(a,b):xs) grid 
    | x == a = putLines ((a,b):xs) $ foldr (Map.adjust (const True)) grid (zip (repeat x) (if y < b then [y..b] else [b..y]))
    | y == b = putLines ((a,b):xs) $ foldr (Map.adjust (const True)) grid (zip (if x < a then [x..a] else [a..x]) (repeat y))

getPairs :: String -> [(Int,Int)]
getPairs xs = map f (splitOn " -> " xs)
    where 
        f ys = case splitOn "," ys of
            [x, y] -> (read x, read y)
            x -> error $ "no parse " ++ show x
