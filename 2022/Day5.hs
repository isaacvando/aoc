import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List
import Data.Char

main :: IO ()
main = do
    input <- fmap lines (readFile "input/5.txt")
    let crates = getCrates input
    let inst = map getInst (drop 10 input)
    putStrLn $ map head $ foldl' (\acc [x,y,z] -> move x y z acc) crates inst
    putStrLn $ map head $ foldl' (\acc [x,y,z] -> move' x y z acc) crates inst

getCrates :: [String] -> [String]
getCrates = map (filter (/= ' ')) . transpose . map parseCrates . take 8

parseCrates :: String -> String
parseCrates (_:c:_:_:xs) = c : parseCrates xs
parseCrates (_:c:_:[]) = [c]

getInst :: String -> [Int]
getInst = map read . filter (all isDigit) . words

move :: Int -> Int -> Int -> [String] -> [String]
move 0 _ _ crates = crates
move count source target crates = move (count-1) source target $
    let (xs,y:ys) = splitAt (source-1) crates 
        in (let (as, b:bs) = splitAt (target-1) (xs ++ [tail y] ++ ys) 
                in as ++ [(head y):b] ++ bs)

move' :: Int -> Int -> Int -> [String] -> [String]
move' count source target crates =
    let (xs,y:ys) = splitAt (source-1) crates 
        in (let (as, b:bs) = splitAt (target-1) (xs ++ [drop count y] ++ ys) 
                in as ++ [chunk ++ b] ++ bs)
    where chunk = take count (crates !! (source-1))
