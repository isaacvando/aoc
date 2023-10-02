-- This is such a fun problem!!
-- import Data.List
-- import Data.List.Split

-- main :: IO ()
-- main = do
--     input <- readFile "input/10.txt"
--     let insts = lines input >>= (getInst . words)
--     let states = scanl' (\acc f -> f acc) 1 insts
--     print $ sum $ map (\x -> (states !! x) * (x+1)) [19,59..219]
--     mapM_ putStrLn (chunksOf 40 $ map (\x -> if abs ((states !! x) - (x `mod` 40)) <= 1 then '#' else ' ') [0..40*6 - 1])
    
-- getInst :: [String] -> [Int -> Int]
-- getInst ["addx", n] = [id, (+ read n)]
-- getInst ["noop"] = [id]

import Data.List
import Data.List.Split

main :: IO ()
main = do
    input <- readFile "input/10.txt"
    putStrLn (solve input)

solve :: String -> String
solve = render . drawSprite . getStates . parse

parse :: String -> [Int -> Int]
parse input = concatMap go (lines input)
    where 
        go :: String -> [Int -> Int]
        go x = case words x of
            ["addx", n] -> [id, (+ read n)]
            ["noop"] -> [id]

getStates :: [Int -> Int] -> [Int]
getStates funcs = go funcs 1
    where
        go :: [Int -> Int] -> Int -> [Int]
        go [] _ = []
        go (f:fs) curr = f curr : go fs (f curr)

drawSprite :: [Int] -> String
drawSprite states = map go (zip states [1..])
    where
        go (state, cycle) = 
            if abs (state - (cycle `mod` 40)) <= 1 
                then '#' 
                else ' '

render :: String -> String
render xs = intercalate "\n" (chunksOf 40 xs)
