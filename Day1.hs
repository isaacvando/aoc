import Data.List.Split
import Data.List

main :: IO ()
main = do
    input <- getContents
    print $ (maximum . map sum . map (map (\x -> read x :: Integer)) . map lines . splitOn "\n\n") input
    print $ (sum . take 3 . reverse . sort . map sum . map (map (\x -> read x :: Integer)) . map lines . splitOn "\n\n") input