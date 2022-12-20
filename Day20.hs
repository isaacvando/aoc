import Data.CircularList
import Data.List
import Data.Maybe

type Circ = CList (Int,Int)

main :: IO ()
main = do
    input <- readFile "input/20.txt"
    let nums = map (\x -> read x :: Int) (lines input)
    let circ = fromList (zip nums [1..]) :: Circ
    print $ getSum (shift (size circ - 1) circ (length nums))
    
    let circ' = fromList (zip (map (*811589153) nums) [1..]) :: Circ
    print $ getSum $ foldr (\_ acc -> shift (size circ - 1) acc (length nums)) circ' [1..10]

getSum :: Circ -> Int
getSum c = val c1 + val c2 + val c3
    where 
        home = fromJust $ findRotateTo (\x -> fst x == 0) c
        c1 = rotN 1000 home
        c2 = rotN 2000 home
        c3 = rotN 3000 home
        val x = fst (fromJust (focus x))


shift :: Int -> Circ -> Int -> Circ
shift circSize circ n = foldl' (f circSize) circ [1..n]
    where
        f s c i = insertL curr (rotN (fst curr `mod` s) (removeL c'))
            where
                curr = fromJust (focus c')
                c' = fromJust (findRotateTo (\x -> snd x == i) c)
