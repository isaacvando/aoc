import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Tree = Dir String [Tree] | File Int | Name deriving (Eq, Show)

type Parser = Parsec Void String

main :: IO ()
main = do
    input <- readFile "foo.txt"
    case parse dir "" input of
        Left x -> putStrLn $ errorBundlePretty x 
        Right x -> print x

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
