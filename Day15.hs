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
