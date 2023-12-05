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
    cards = parse file
    Stdout.line "Part 1: \(part1 cards)\nPart 2: \(part2 cards)"

Card : {id: Nat, winners: Set Nat, actuals: List Nat}

part1 = \input ->
    input
    |> List.map countCard
    |> List.sum
    |> Num.toStr

part2 = \input -> 
    collectCards input
    |> Num.toStr

collectCards = \cards -> 
    counts = List.map cards \c -> {card: c, count: 1}
    collectCardsHelp counts 0

collectCardsHelp = \counts, total -> 
    when counts is
        [] -> total
        [{card, count}, .. as rest] -> 
            wins = getWinCount card
            prizeCards = 
                List.sublist rest {start: 0, len: wins}
                |> List.map \c -> 
                    {c & count: c.count + count}

            remaining = prizeCards |> List.concat (List.dropFirst rest wins)

            collectCardsHelp remaining (total + count)

countCard : Card -> U64
countCard = \card -> 
    when getWinCount card is
        0 -> 0
        count -> 
            exponent = count - 1 |> Num.toU64
            Num.powInt 2 exponent

getWinCount : Card -> Nat 
getWinCount = \{id, winners, actuals} -> 
    List.countIf actuals \elem -> Set.contains winners elem
    
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

unwrap = \r -> 
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val
