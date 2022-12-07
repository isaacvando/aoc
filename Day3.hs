import qualified Data.Set as S
import Data.List

main :: IO ()
main = do
    input <- readFile "input/3.txt"
    print $ solve input
    print $ solve' input

solve' :: String -> Int
solve' = sum
    . map getValue
    . concat
    . map (\(x,y,z) -> S.toList $ S.intersection (S.intersection (S.fromList x) (S.fromList y)) (S.fromList z))
    . groupBags
    . lines

solve :: String -> Int
solve = sum
    . map getValue
    . concat
    . map (\(x,y) -> S.toList $ S.intersection (S.fromList x) (S.fromList y))
    . map (\x -> splitAt ((length x) `div` 2) x) 
    . lines

letters = ['_'] ++ ['a'..'z'] ++ ['A'..'Z']

getValue :: Char -> Int
getValue c = case elemIndex c letters of
    Just x -> x
    Nothing -> error (c:" is no good")

groupBags :: [String] -> [(String, String, String)]
groupBags [] = []
groupBags (x:y:z:xs) = (x, y, z) : groupBags xs