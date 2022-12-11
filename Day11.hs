import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List

data Monkey = Monkey [Integer] (Integer -> Integer) (Integer -> Bool) Integer Integer 

instance Show Monkey where
    show (Monkey list _ _ n1 n2) = "Monkey " ++ show list ++ " " ++ show n1 ++ " " ++ show n2

type Parser = Parsec Void String

main :: IO ()
main = do
    input <- readFile "input/11.txt"
    let monkeys = parseMonkeys input
    let activities = snd $ foldr (\_ (ms,counts) -> doRound ms counts) (monkeys,(replicate (length monkeys) 0)) [1..20]
    let activities' = snd $ foldr (\_ (ms,counts) -> doRound' ms counts) (monkeys,(replicate (length monkeys) 0)) [1..10000]
    print $ let (x:y:_) = reverse $ sort activities in x * y
    print $ let (x:y:_) = reverse $ sort activities' in x * y

-- doRound :: [Monkey] -> [Integer] -> ([Monkey],[Integer])
-- doRound ms counts = f ms counts 0
--     where 
--         f ms counts i 
--             | i == length ms = (ms,counts)
--             | otherwise = let (ms', count') = doTurn ms (counts !! i) i in f ms' (updateElt counts i count') (i+1)

doRound :: [Monkey] -> [Integer] -> ([Monkey],[Integer])
doRound ms counts = f ms counts 0
    where 
        f :: [Monkey] -> [Integer] -> Integer -> ([Monkey],[Integer])
        f ms counts i 
            | i == fromIntegral (length ms) = (ms,counts)
            | otherwise = let (ms', count') = doTurn ms (counts !! (fromIntegral i)) i in f ms' (updateElt counts i count') (i+1)

doRound' :: [Monkey] -> [Integer] -> ([Monkey],[Integer])
doRound' ms counts = f ms counts 0
    where 
        f :: [Monkey] -> [Integer] -> Integer -> ([Monkey],[Integer])
        f ms counts i 
            | i == fromIntegral (length ms) = (ms,counts)
            | otherwise = let (ms', count') = doTurn' ms (counts !! (fromIntegral i)) i in f ms' (updateElt counts i count') (i+1)

doTurn :: [Monkey] -> Integer -> Integer -> ([Monkey],Integer)
doTurn ms count i = case ms !! (fromIntegral i) of
    m@(Monkey [] op test i1 i2) -> (ms,count)
    (Monkey (x:xs) op test i1 i2) -> doTurn newMonkeys  (count+1) i
        where 
            newMonkeys = (updateElt (updateElt ms i (Monkey xs op test i1 i2)) newMonkey (appendItem (ms !! (fromIntegral newMonkey)) newWorry))
            (newWorry, newMonkey) = let newItem = (floor (fromIntegral (op x) / 3.0)) `mod` 9699690 in (if test newItem then (newItem,i1) else (newItem,i2))

doTurn' :: [Monkey] -> Integer -> Integer -> ([Monkey],Integer)
doTurn' ms count i = case ms !! (fromIntegral i) of
    m@(Monkey [] op test i1 i2) -> (ms,count)
    (Monkey (x:xs) op test i1 i2) -> doTurn' newMonkeys  (count+1) i
        where 
            newMonkeys = (updateElt (updateElt ms i (Monkey xs op test i1 i2)) newMonkey (appendItem (ms !! (fromIntegral newMonkey)) newWorry))
            (newWorry, newMonkey) = let newItem = (op x) `mod` 9699690 in (if test newItem then (newItem,i1) else (newItem,i2))

updateElt :: [a] -> Integer -> a -> [a]
updateElt xs i x = let (a,_:b) = splitAt (fromIntegral i) xs in a ++ (x:b)

appendItem :: Monkey -> Integer -> Monkey
appendItem (Monkey xs f g n m) x = Monkey (xs ++ [x]) f g n m

parseMonkeys :: String -> [Monkey]
parseMonkeys xs = case parse (some monkey) "" xs of
    Left bundle -> error $ errorBundlePretty bundle
    Right x -> x

monkey :: Parser Monkey 
monkey = do
    string "Monkey " <* number <* char ':' <* newline
    items <- string "  Starting items: " *> (some (number <* optional (string ", "))) <* newline
    op <- string "  Operation: new = old " *> operation <* newline
    test <- string "  Test: divisible by " *> test <* newline
    n1 <- string "    If true: throw to monkey " *> number <* newline
    n2 <- string "    If false: throw to monkey " *> number <* newline
    newline
    return $ Monkey items op test n1 n2

test :: Parser (Integer -> Bool)
test = do
    n <- number
    return (\x -> x `mod` n == 0)

operation :: Parser (Integer -> Integer)
operation = do
    operator <- oneOf "+*" <* hspace
    num <- string "old" <|> some digitChar
    let op = case operator of '+' -> (+); '*' -> (*)
    return $ case num of "old" -> (\x -> x `op` x); n -> (`op` (read num))

number :: Parser Integer
number = do
    n <- some digitChar
    return $ read n
