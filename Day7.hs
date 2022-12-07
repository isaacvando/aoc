import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Tree = Dir String [Tree] | File Int | Name deriving (Eq, Show)

type Parser = Parsec Void String

main :: IO ()
main = do
    input <- readFile "7.txt"
    case parse dir "" input of
        Left x -> putStrLn $ errorBundlePretty x 
        Right x -> do
            print $ (sum . filter (<= 100000) . listSizes) x
            print $ (minimum . filter (\x -> availableSpace + x >= 30000000) . listSizes) x
                where availableSpace = 70000000 - maximum (listSizes x)

dir :: Parser Tree
dir = do
    name <- try $ string "$ cd " *> some (noneOf ".\n") <* newline <* string "$ ls" <* newline
    elts <- many (file <|> dir <|> dirName)
    optional $ string "$ cd .." <* newline
    return $ Dir name (filter (/= Name) elts)

file :: Parser Tree
file = do
    n <- some digitChar <* some (noneOf "\n") <* newline
    return $ File (read n)

dirName :: Parser Tree
dirName = return Name <* string "dir " <* some (noneOf "\n") <* newline

getSize :: Tree -> Int
getSize (File n) = n
getSize (Dir _ trees) = foldr (\x acc -> getSize x + acc) 0 trees

listSizes :: Tree -> [Int]
listSizes t@(Dir _ trees) = getSize t : (trees >>= listSizes)
listSizes _ = []
