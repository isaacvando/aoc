import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad
import Control.Arrow
import Data.List.Extra

-- main :: IO ()
-- main = do
--     input <- getContents
--     case parse p "" input of
--         Left bundle -> putStrLn $ errorBundlePretty bundle
--         Right x -> print $ (
--             filter (\((x1,x2), (y1,y2)) -> x1 <= y1 && y2 <= x2 || y1 <= x1 && x2 <= y2)
--             >>> length ) x

-- p :: Parsec Void String [((Int,Int), (Int,Int))]
-- p = some (do
--     p1 <- pair <* char ','
--     p2 <- pair <* newline
--     return (p1,p2))

-- pair :: Parsec Void String (Int, Int)
-- pair = do
--     n1 <- some digitChar
--     char '-'
--     n2 <- some digitChar
--     return (read n1 :: Int, read n2 :: Int)

main :: IO ()
main = do
    input <- getContents
    case parse p "" input of
        Left bundle -> putStrLn $ errorBundlePretty bundle
        Right x -> print $ (
            filter (\((x1,x2), (y1,y2)) -> notNull $ intersect [x1..x2] [y1..y2])
            >>> length ) x

p :: Parsec Void String [((Int,Int), (Int,Int))]
p = some (do
    p1 <- pair <* char ','
    p2 <- pair <* newline
    return (p1,p2))

pair :: Parsec Void String (Int, Int)
pair = do
    n1 <- some digitChar
    char '-'
    n2 <- some digitChar
    return (read n1 :: Int, read n2 :: Int)