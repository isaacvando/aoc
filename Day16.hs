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
    -- input <- readFile "foo.txt"
    input <- readFile "input/16.txt"
    let valves = map getValve (lines input)
    let graph = M.fromList valves
    let paths = foldl' (\acc _ -> refine graph (concatMap (steps graph) acc)) [[Goto "AA"]] [1..30]
    print $ getValue graph (getMax graph paths)

refine :: Graph -> [Path] -> [Path]
refine graph paths = foldr (\x acc -> (getMax graph (collect x)) : acc) [] symbols
    where 
        symbols = nub (map head paths)
        collect symb = filter (\x -> head x == symb) paths

getMax :: Graph -> [Path] -> Path
getMax graph paths = f paths (-1) []
    where 
        f [] _ result = result
        f (x:xs) n result = let v = getValue graph x in if v > n then f xs v x else f xs n result

getValue :: Graph -> Path -> Int
getValue graph path = f (reverse path) 0 0
    where
        f [] total _ = total
        f (x:xs) total n = case x of
            Goto _ -> f xs total (n+1)
            Open loc -> f xs (total + (flow * (30 - n))) (n+1)
                where flow = fst (fromJust (M.lookup loc graph))

steps :: Graph -> Path -> [Path]
steps _ [] = error "wtf"
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
