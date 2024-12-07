app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.File

main =
    input = File.readUtf8! "input/5.txt" |> Str.trim
    Stdout.line!
        """
        Part 1: $(Inspect.toStr (part1 input))
        Part 2: $(Inspect.toStr (part2 input))
        """

part1 : Str -> Result U64 _
part1 = \input ->
    {pairs, edits} = parse? input
    befores : Dict U64 (Set U64)
    befores = List.walk pairs (Dict.empty {}) \dict, pair ->
        Dict.update dict pair.0 \r ->
            when r is
                Err _ -> Set.fromList [pair.1] |> Ok
                Ok set -> Set.insert set pair.1 |> Ok
    List.keepIf edits \edit ->
        isValid edit befores
    |> List.mapTry? \edit ->
        List.get edit (List.len edit // 2)
    |> List.sum
    |> Ok

isValid : List U64, Dict U64 (Set U64) -> Bool
isValid = \edit, befores ->
    List.walkTry edit (Set.empty {}) \seen, page ->
        mustBeBefore = Dict.get befores page |> Result.withDefault (Set.empty {})
        if Set.intersection mustBeBefore seen |> Set.isEmpty then
            Set.insert seen page |> Ok
        else
            Err InvalidEdit
    |> Result.isOk

part2 = \input ->
    "wip"

parse : Str -> Result {pairs: List (U64, U64), edits: List (List U64)} _
parse = \input ->
    {before, after} = Str.splitFirst? input "\n\n"
    pairs =
        Str.splitOn before "\n"
        |> List.mapTry \line ->
            pair = Str.splitFirst? line "|"
            first = Str.toU64? pair.before
            second = Str.toU64? pair.after
            Ok (first, second)
        |> try
    edits =
        Str.splitOn after "\n"
        |> List.mapTry \line ->
            Str.splitOn line "," |> List.mapTry Str.toU64
        |> try
    Ok {pairs, edits}

expect
    result = part1 sampleInput
    result == Ok 143

sampleInput =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """
