import Data.List
import Data.List.Split
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe

type Parser = Parsec Void String

data Quality = Good | Bad | Idk deriving (Eq,Show)

newtype Packet = Packet String deriving (Eq,Show)

instance Ord Packet where
    compare (Packet p) (Packet q) = if check [p] [q] == Good then GT else LT

main :: IO ()
main = do
    input <- readFile "input/13.txt"
    let pairs = (splitOn [""] . lines) input
    let results = map (\[x,y] -> check [x] [y]) pairs
    let packets = [Packet "[[2]]",Packet "[[6]]"] ++ concatMap (map Packet) pairs
    print $ sum $ zipWith (\b n -> if b == Good then n else 0) results [1..]
    let getIndex p = fromJust (elemIndex p (reverse (sort packets))) + 1
    print $ getIndex (Packet "[[2]]") * getIndex (Packet "[[6]]")

check :: [String] -> [String] -> Quality
check [] [] = Idk
check _ [] = Bad
check [] _ = Good
check (l:ls) (r:rs)
    | l == r = check ls rs
    | all isDigit l && all isDigit r = if (read l :: Int) < read r then Good else Bad
    | otherwise = case check (separate l) (separate r) of
        Idk -> check ls rs
        x -> x

separate :: String -> [String]
separate xs = case parse chunks "" (peel xs) of
    Left e -> error $ errorBundlePretty e
    Right x -> x
    where
        peel xs@('[':y:_) = (init . tail) xs
        peel x = x

chunks :: Parser [String]
chunks = many ((some digitChar <|> list) <* optional (char ','))

list :: Parser String
list = do
    char '['
    xs <- many ((some digitChar <|> list) <* optional (char ','))
    char ']'
    return $ "[" ++ intercalate "," xs ++ "]"
