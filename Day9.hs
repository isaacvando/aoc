import Data.Char
import qualified Data.Set as Set
import Data.Bifunctor
import Data.List

data Move = R | L | U | D deriving (Show, Eq)
type Pos = (Int,Int)

main :: IO ()
main = do
    input <- readFile "input/9.txt"
    let moves = (concat . map (getSingleMoves . words) . lines) input
    print $ Set.size $ move Set.empty moves (0,0) (0,0)
    print $ Set.size $ move' Set.empty moves (map (const (0,0)) [1..10])
    where
        getSingleMoves [c, n] = replicate (read n) (case c of 
            "R" -> R
            "L" -> L
            "U" -> U
            "D" -> D)

move :: Set.Set Pos -> [Move] -> Pos -> Pos -> Set.Set Pos
move visited [] _ t = Set.insert t visited
move visited (m:moves) h t = move (Set.insert t visited) moves h' (moveTail h' t)
    where h' = moveHead m h

moveHead :: Move -> Pos -> Pos
moveHead m = case m of
    R -> first (+1)
    L -> first (+(-1))
    U -> second (+1)
    D -> second (+(-1))

moveTail :: Pos -> Pos -> Pos
moveTail h@(h1,h2) t@(t1,t2)
    | dist h t <= 1 = t
    | h1 == t1 && h2 < t2 = (t1,t2-1)
    | h1 == t1 && h2 > t2 = (t1,t2+1)
    | h2 == t2 && h1 < t1 = (t1-1,t2)
    | h2 == t2 && h1 > t1 = (t1+1,t2)
    | t1 - h1 == 2 = (h1+1,h2)
    | h1 - t1 == 2 = (h1-1,h2)
    | t2 - h2 == 2 = (h1,h2+1)
    | h2 - t2 == 2 = (h1,h2-1)
    | otherwise = error $ "head: " ++ show h ++ " tail: " ++ show t
    where dist (a,b) (x,y) = max (abs (a - x)) (abs (b - y))

moveTail' :: Pos -> Pos -> Pos
moveTail' h@(h1,h2) t@(t1,t2)
    | dist h t <= 1 = t
    | h1 == t1 && h2 < t2 = (t1,t2-1)
    | h1 == t1 && h2 > t2 = (t1,t2+1)
    | h2 == t2 && h1 < t1 = (t1-1,t2)
    | h2 == t2 && h1 > t1 = (t1+1,t2)
    | t1 - h1 == 2 && t2 - h2 == 2 = (h1+1,h2+1)
    | h1 - t1 == 2 && h2 - t2 == 2 = (h1-1,h2-1)
    | t1 - h1 == 2 && h2 - t2 == 2 = (h1+1,h2-1)
    | h1 - t1 == 2 && t2 - h2 == 2 = (h1-1,h2+1)
    | t1 - h1 == 2 = (h1+1,h2)
    | h1 - t1 == 2 = (h1-1,h2)
    | t2 - h2 == 2 = (h1,h2+1)
    | h2 - t2 == 2 = (h1,h2-1)
    | otherwise = error $ "head: " ++ show h ++ " tail: " ++ show t
    where dist (a,b) (x,y) = max (abs (a - x)) (abs (b - y))

move' :: Set.Set Pos -> [Move] -> [Pos] -> Set.Set Pos
move' visited [] rope = Set.insert (last rope) visited
move' visited (m:moves) rope = move' (Set.insert (last rope') visited) moves rope'
    where rope' = moveRope m rope

moveRope :: Move -> [Pos] -> [Pos]
moveRope m (h:ps) = foldl' (\acc x -> acc ++ [moveTail' (last acc) x]) [h'] ps
    where h' = moveHead m h
