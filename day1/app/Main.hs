module Main where

import Data.List.Split

main :: IO ()
main = do
    input <- getContents
    print $ (maximum . map sum . map (map (\x -> read x :: Integer)) . map lines . splitOn "\n\n") input
