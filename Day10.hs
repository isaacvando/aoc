import Data.List

main :: IO ()
main = do
    input <- readFile "input/10.txt"
    let insts = id : (concat . map (getInst . words) . lines) input -- appended id for 1 based indexing
    let states = scanl' (\acc f -> f acc) 1 insts
    print $ sum $ map (\x -> (states !! x) * x) [20,60..220]
    

getInst :: [String] -> [Int -> Int]
getInst ["addx", n] = [id, (+ read n)]
getInst ["noop"] = [id]