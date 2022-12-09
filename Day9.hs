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

move' :: Set.Set Pos -> [Move] -> [Pos] -> Set.Set Pos
move' visited [] rope = Set.insert (last rope) visited
move' visited (m:moves) rope = move' (Set.insert (last rope') visited) moves rope'
    where rope' = moveRope m rope

moveHead :: Move -> Pos -> Pos
moveHead m = case m of
    R -> first (+1)
    L -> first (+(-1))
    U -> second (+1)
    D -> second (+(-1))

moveTail :: Pos -> Pos -> Pos
moveTail (h1,h2) (t1,t2) 
    | dist (h1,h2) (t1,t2)  <= 1 = (t1,t2)
    | otherwise = (adjust h1 t1, adjust h2 t2)
    where 
        dist (a,b) (x,y) = max (abs (a - x)) (abs (b - y))
        adjust h t
            | h < t && t - h >= 2 = h+1
            | h > t && h - t >= 2 = h-1
            | otherwise = h

moveRope :: Move -> [Pos] -> [Pos]
moveRope m (h:ps) = foldl' (\acc x -> acc ++ [moveTail (last acc) x]) [h'] ps
    where h' = moveHead m h
