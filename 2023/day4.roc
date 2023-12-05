app "day4"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core,
        parser.String,
        "input/4.txt" as file : Str,
        "input/4ex.txt" as example : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    # Stdout.line "Part 1: \(part1 file)\nPart 2: \(part2 file)"
    dbg part1 file
    Task.ok {}

Card : {id: Nat, winners: Set Nat, actuals: List Nat}

part1 = \input ->
    parse input
    |> List.map countCard
    |> List.sum

countCard : Card -> U64
countCard = \{id, winners, actuals} -> 
    winCount = List.countIf actuals \elem -> 
        Set.contains winners elem

    when winCount is
        0 -> 0
        count -> 
            exponent = count - 1 |> Num.toU64
            Num.powInt 2 exponent
    


parse : Str -> List Card
parse = \input -> 
    spaces = Core.chompWhile \c -> c == ' '
    numbers = Core.sepBy1 String.digits spaces

    toCard = \id -> \winners -> \actuals -> 
        {id, winners: Set.fromList winners, actuals}
    # could this use record builder syntax?
    card = 
        Core.const toCard
        |> Core.skip (String.string "Card")
        |> Core.skip spaces
        |> Core.keep String.digits
        |> Core.skip (String.string ":")
        |> Core.skip spaces
        |> Core.keep numbers
        |> Core.skip spaces
        |> Core.skip (String.string "|")
        |> Core.skip spaces
        |> Core.keep numbers

    cards = Core.sepBy1 card (String.scalar '\n')

    String.parseStr cards input |> unwrap


part2 = \input -> "-"

unwrap = \r -> 
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x -> 
    dbg x
    x
