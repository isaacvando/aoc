app "day2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core,
        parser.String,
        "input/2.txt" as file : Str,
        "input/2ex.txt" as example : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    Stdout.line "Part 1: \(part1 file)\nPart 2: \(part2 file)"

part1 = \input -> 
    parse input 
    |> unwrap
    |> List.dropIf isImpossible
    |> List.map \Game id _ -> id
    |> List.sum
    |> Num.toStr
    

isImpossible = \Game _ list -> 
    (Red r, Green g, Blue b) = getTotalMax list
    r > 12 || g > 13 || b > 14

getTotalMax = \list -> 
    List.walk list (Red 0, Green 0, Blue 0) \(Red r, Green g, Blue b), elem -> 
        (Red x, Green y, Blue z) = getMaxForRound elem
        (Red (Num.max x r), Green (Num.max y g), Blue (Num.max b z))

getMaxForRound = \list -> 
    List.walk list (Red 0, Green 0, Blue 0) \(Red x, Green y, Blue z), elem -> 
        when elem is
            Red n if n > x -> (Red n, Green y, Blue z)
            Green n if n > y -> (Red x, Green n, Blue z)
            Blue n if n > z -> (Red x, Green y, Blue n)
            _ -> (Red x, Green y, Blue z)


parse = \input -> 
    String.parseStr lines input

lines = 
    Core.sepBy line (String.string "\n")

expect 
    result = String.parseStr lines "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    Result.isOk result

line = 
    collect = \id -> \r -> 
        Game id r
    Core.const collect
        |> Core.skip (String.string "Game ")
        |> Core.keep String.digits
        |> Core.skip (String.string ": ")
        |> Core.keep rounds

rounds = 
    Core.sepBy1 round (String.string "; ")

expect 
    result = String.parseStr line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    result == Ok (Game 1 [[Blue 3, Red 4], [Red 1, Green 2, Blue 6], [Green 2]])

round =
    Core.sepBy1 color (String.string ", ")

expect 
    result = String.parseStr round "100 red, 50 blue, 78 green" 
    result == Ok [Red 100, Blue 50, Green 78]

color = 
    Core.oneOf [red, blue, green]

expect 
    result = String.parseStr color "57 blue"
    result == Ok (Blue 57)

blue = 
    Core.const (\x -> Blue x)
    |> Core.keep String.digits
    |> Core.skip (String.string " blue")

green = 
    Core.const (\x -> Green x)
    |> Core.keep String.digits
    |> Core.skip (String.string " green")

red = 
    Core.const (\x -> Red x)
    |> Core.keep String.digits
    |> Core.skip (String.string " red")

expect String.parseStr red "100 red" == Ok (Red 100)

# parseLine = \line -> 
#     when Str.toUtf8 line is
#         ['G','a','m','e',':', ' ', x]


part2 = \input -> "-"

unwrap = \r -> 
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x -> 
    dbg x
    x
