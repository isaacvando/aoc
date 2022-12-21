import qualified Data.Map as M
import Data.Maybe (fromJust)

data Monkey = Number String Int | Function String String (Int -> Int -> Int) String

main :: IO ()
main = do
    -- input <- readFile "foo2.txt"
    input <- readFile "input/21.txt"
    let monkeys = map parse (lines input)
    let vals = M.fromList $ map (\(Number name val) -> (name,val)) (filter isNumber monkeys)
    let root = findRoot vals (filter (not . isNumber) monkeys)
    print root

-- this function will hang if there is not a naive way to satisfy the system
findRoot :: M.Map String Int -> [Monkey] -> Int
findRoot vals [] = fromJust (M.lookup "root" vals)
findRoot vals (x@(Function name m1 f m2):xs) 
    | M.member "root" vals = fromJust (M.lookup "root" vals)
    | M.member m1 vals && M.member m2 vals = findRoot (M.insert name (f v1 v2) vals) xs
    | otherwise = findRoot vals (xs ++ [x])
        where
            v1 = fromJust (M.lookup m1 vals)
            v2 = fromJust (M.lookup m2 vals)

parse :: String -> Monkey
parse xs = case words xs of
    [name, monkey1, op, monkey2] -> Function (init name) monkey1 (getFunc op) monkey2
    [name, num] -> Number (init name) (read num)

getFunc :: String -> (Int -> Int -> Int)
getFunc "/" = div
getFunc "+" = (+)
getFunc "-" = (-)
getFunc "*" = (*)

isNumber :: Monkey -> Bool
isNumber (Number _ _) = True
isNumber _ = False
