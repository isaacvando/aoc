module Main where

import qualified Data.Set as S
import Data.List

-- main :: IO ()
-- main = do
--     input <- getContents
--     print $ (
--         sum
--         . map getValue
--         . concat
--         . map (\(x,y) -> S.toList $ S.intersection (S.fromList x) (S.fromList y))
--         . map (\x -> splitAt ((length x) `div` 2) x) 
--         . lines) input

-- letters = ['_'] ++ ['a'..'z'] ++ ['A'..'Z']

-- getValue :: Char -> Int
-- getValue c = case elemIndex c letters of
--     Just x -> x
--     Nothing -> error (c:" is no good")

main :: IO ()
main = do
    input <- getContents
    print $ (
        sum
        . map getValue
        . concat
        . map (\(x,y,z) -> S.toList $ S.intersection (S.intersection (S.fromList x) (S.fromList y)) (S.fromList z))
        . groupBags
        . lines) input

letters = ['_'] ++ ['a'..'z'] ++ ['A'..'Z']

getValue :: Char -> Int
getValue c = case elemIndex c letters of
    Just x -> x
    Nothing -> error (c:" is no good")

groupBags :: [String] -> [(String, String, String)]
groupBags [] = []
groupBags (x:y:z:xs) = (x, y, z) : groupBags xs