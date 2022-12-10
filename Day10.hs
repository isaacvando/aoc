-- This is such a fun problem!!
import Data.List
import Data.List.Split

main :: IO ()
main = do
    input <- readFile "input/10.txt"
    let insts = lines input >>= (getInst . words)
    let states = scanl' (\acc f -> f acc) 1 insts
    print $ sum $ map (\x -> (states !! x) * (x+1)) [19,59..219]
    mapM_ putStrLn (chunksOf 40 $ map (\x -> if abs ((states !! x) - (x `mod` 40)) <= 1 then '#' else ' ') [0..40*6 - 1])
    
getInst :: [String] -> [Int -> Int]
getInst ["addx", n] = [id, (+ read n)]
getInst ["noop"] = [id]
