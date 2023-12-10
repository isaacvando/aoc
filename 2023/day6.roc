app "day6"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{Parser},
        parser.String,
        "input/6.txt" as file : Str,
        "input/6ex.txt" as example : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    # Stdout.line "Part 1: \(part1 file)\nPart 2: \(part2 file)"
    dbg (parse example)
    Task.ok {}


part1 = \input -> "_"

part2 = \input -> "_"

parse = \input -> 
    eatNonDigits = Core.chompWhile \c -> 
        !(List.contains ['0','1','2','3','4','5','6','7','8','9'] c)

    numbers : Parser * (List Nat)
    numbers = Core.const \x -> x
        |> Core.skip eatNonDigits
        |> Core.keep (Core.sepBy1 String.digits eatNonDigits)

    collectPage : List Nat -> (List Nat -> List {time: Nat, dist: Nat})
    collectPage = \t -> \d -> 
        List.map2 t d \x, y -> 
            {time: x, dist: y}

    page = Core.const collectPage
        |> Core.keep numbers
        |> Core.keep numbers

    String.parseStr page input


unwrap = \r -> 
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x -> 
    dbg x
    x
