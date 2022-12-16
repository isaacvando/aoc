import Data.List.Split
import Control.Arrow
import Data.List
import qualified Data.IntervalSet as I
import Data.Interval

type Pos = (Int,Int)
type Circle = (Pos,Int)

main :: IO ()
main = do
    input <- readFile "input/15.txt"
    let pairs = map parseLine (lines input)
    let flat = concatMap (\(p,q) -> [p,q]) pairs
    let (minX,maxX) = (minimum &&& maximum) (map fst flat)
    let circles = zip (map fst pairs) (map (uncurry manhat) pairs)
    let maxR = maximum (map snd circles)
    print $ length $ filter (isOutside circles) (zip [minX-maxR..maxX+maxR] (repeat 2000000)) \\ (map snd pairs)
    let ([i,_],r) = head [(x,row) | row <- [0..4000000 :: Int], let x = I.toList (intervals circles row), x /= [Finite 0 <=..<= Finite 4000000]]
    -- print $ let (Finite l) = lowerBound i in (l + 1) * 4000000 + r
    print $ let Finite b = upperBound i in (b+1) * 4000000 + r

intervals :: [Circle] -> Int -> I.IntervalSet Int
intervals cs row = f cs I.empty
    where
        f [] ints = ints
        f (((x,y),r):xs) ints = f xs (I.insert int ints)
            where
                int = fromIntegral (max 0 (x - adjRad)) <=..<= fromIntegral (min 4000000 (x + adjRad))
                adjRad = r - abs (y - row)

isOutside :: [Circle] -> Pos -> Bool
isOutside [] _ = True
isOutside ((center,radius):cs) pos = manhat center pos > radius && isOutside cs pos

-- Manhattan (Taxicab) distance metric
manhat :: Pos -> Pos -> Int
manhat (x,y) (a,b) = abs (x-a) + abs (y-b)

parseLine :: String -> (Pos,Pos)
parseLine xs = let [a,b,c,d] = map read nums in ((a,b),(c,d))
    where nums = splitOneOf ",:" (filter (`elem` "-0123456789,:") xs)


    -- print $ length (row (minX-maxR) (maxX+maxR) 2000000 \\ (map snd pairs))
    -- let rows = map (row 0 4000000) [0..40]
    -- print $ filter (\x -> length x == 1) rows
    -- print $ length $ row 0 4000000 20000
    -- let grid = Set.fromList [(x,y) | x <- [0..4000000], y <- [0..4000000]]
    -- let viable =
    -- let foo = filter (isOutside circles) (concat [[(x,y) | x <- [0..400]] | y <- [0..4000000]])
    -- let grid = concat [[(x,y) | x <- [0..4000000]] | y <- [0..4000000]]
    -- print $ Set.size $ removeArea grid (head circles)
    -- print foo
    -- print $ Set.size (grid Set.\\ (Set.fromList [(x,y) | x <- [0..400000], y <- [0..4000000]]))



-- position :: [Circle] -> Int -> Maybe Pos

-- intervals :: [Circle] -> Int -> [(Int,Int)]
-- intervals cs row = f cs [(0,0)]
--     where 
--         f [] ints = ints
--         f _ [(0,4000000)] = [(0,4000000)]
--         f (((x,y),r):xs) ints = f xs (merge (int:ints))
--             where 
--                 int = (r - abs (y - row), r + abs (y - row))
                
-- merge :: [(Int,Int)] -> [(Int,Int)]   
-- merge xs = foldr f []
--     | p


-- removeArea :: Set.Set Pos -> Circle -> Set.Set Pos
-- removeArea grid ((x,y),r) = foldr (\p acc -> if manhat (x,y) p <= r then Set.delete p acc else acc) grid region
--     where region = [(a,b) | a <- [x-r..x+r], b <- [y-r,y+r]]
