import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M
import Data.Maybe
import Data.List

data Action = Goto String | Open String deriving (Show,Eq)
type Path = [Action]
type Graph = M.Map String (Int,[String])

main :: IO ()
main = do
    input <- readFile "foo.txt"
    -- input <- readFile "input/16.txt"
    let valves = map getValve (lines input)
    let graph = M.fromList valves
    let start = map ((:[]) . Goto . fst) valves
    let paths = foldl' (\acc _ -> refine graph (concatMap (steps graph) acc)) start [1..30]
    print $ getValue graph (getMax graph paths)
    -- print $ (getMax graph paths)

    -- print $ getValue graph ( reverse [Goto "DD", Open "DD", Goto "CC", Goto "BB", Open "BB", Goto "AA", Goto "II", Goto "JJ", Open "JJ", Goto "II", Goto "AA", Goto "DD", Goto "EE", Goto "FF", Goto "GG", Goto "HH", Open "HH", Goto "GG", Goto "FF", Goto "EE", Open "EE", Goto "DD", Goto "CC", Open "CC"])

refine :: Graph -> [Path] -> [Path]
refine graph paths = foldr (\x acc -> (getMax graph (collect x)) : acc) [] symbols
    where 
        symbols = nub (map head paths)
        collect symb = filter (\x -> head x == symb) paths
        
getMax :: Graph -> [Path] -> Path
getMax graph paths = let xs = map (\x -> (x, getValue graph x)) paths in f xs (-1) []
    where 
        f :: [(Path,Int)] -> Int -> Path -> Path
        f [] m result = result
        f ((p,val):xs) m result = if val > m then f xs val p else f xs m result

getValue :: Graph -> Path -> Int
getValue graph path = f (reverse path) 0 1
    where
        f [] total _ = total
        f (x:xs) total n = case x of
            Goto _ -> f xs total (n+1)
            Open loc -> f xs (total + (flow * (30 - n))) (n+1)
                where flow = fst (fromJust (M.lookup loc graph))

steps :: Graph -> Path -> [Path]
steps graph (x:xs) = filter valid (map (:x:xs) moves)
    where 
        valid path = (opens path) == nub (opens path)
        opens path = filter isOpen path
        neighs n = snd (fromJust (M.lookup n graph))
        moves = case x of
            Open name -> map Goto (neighs name) ++ [Goto name]
            Goto name -> map Goto (neighs name) ++ [Open name, Goto name]

isOpen :: Action -> Bool
isOpen (Open _) = True
isOpen _ = False

getValve :: String -> (String,(Int,[String]))
getValve xs = case parse valve "" xs of
    Left e -> error $ errorBundlePretty e
    Right x -> x

valve :: Parsec Void String (String,(Int,[String]))
valve = do
    name <- string "Valve " *> some (noneOf " ")
    rate <- string " has flow rate=" *> some digitChar
    neighbors <- string "; " *> (string "tunnel " <|> string "tunnels ") 
        *> (string "lead " <|> string "leads ") *> string "to " 
        *> (string "valve " <|> string "valves ") 
        *> some (some letterChar <* optional (string ", "))
    return (name,(read rate,neighbors))

-- 2622
-- answer