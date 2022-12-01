module Main where

import Data.List.Split
import Data.List

-- part 1
-- main :: IO ()
-- main = do
--     input <- getContents
--     print $ (maximum . map sum . map (map (\x -> read x :: Integer)) . map lines . splitOn "\n\n") input

-- part 2
main :: IO ()
main = do
    input <- getContents
    print $ (sum . take 3 . reverse . sort . map sum . map (map (\x -> read x :: Integer)) . map lines . splitOn "\n\n") input
