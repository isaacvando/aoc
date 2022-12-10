
main :: IO ()
main = do
    input <- readFile "foo.txt"
    print input