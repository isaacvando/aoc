import Data.List.Split
import qualified Data.Set as Set

type Pos = (Int,Int,Int)
type Space = Set.Set Pos

main :: IO ()
main = do
    input <- readFile "input/18.txt"
    let coords = (map (\[x,y,z] -> (x,y,z)) . map (map (\ x -> read x :: Int) . splitOn ",") . lines) input
    let blocks = Set.fromList coords
    print $ foldr (\x acc -> acc + countAdj blocks x) 0 coords

    let (minX,minY,minZ) = findExtrema coords minimum
    let (maxX,maxY,maxZ) = findExtrema coords maximum

    -- the +1 and -1 ensure that the surface area of blocks in the outer layer is counted
    let space = Set.fromList [(x,y,z) | x <- [minX-1..maxX+1], y <- [minY-1..maxY+1], z <- [minZ-1..maxZ+1]]
    let shell = [(maxX,a,b) | a <- [minY..maxY], b <- [minZ..maxZ]] ++ [(a,maxY,b) | a <- [minX..maxX], b <- [minZ..maxZ]] ++ [(a,b,maxZ) | a <- [minX..maxX], b <- [minY..maxY]]
    print $ outerArea shell blocks space

outerArea :: [Pos] -> Space -> Space -> Int
outerArea shell blocks space = f shell Set.empty 0
    where 
        f [] _ n = n
        f (pos@(x,y,z):xs) vis n 
            | Set.member pos vis || Set.notMember pos space || Set.member pos blocks = f xs vis n
            | otherwise = f (moves ++ xs) (Set.insert pos vis) (n + 6 - length moves)
            where moves = filter (`Set.notMember` blocks) [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]

findExtrema :: [Pos] -> ([Int] -> Int) -> Pos
findExtrema coords f = (f (map (\(x,_,_) -> x) coords), f (map (\(_,y,_) -> y) coords), f (map (\(_,_,z) -> z) coords))

countAdj :: Set.Set Pos -> Pos -> Int
countAdj blocks (x,y,z) = 6 - length (filter id (map (`Set.member` blocks) moves))
    where moves = [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]
