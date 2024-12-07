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
    {befores, edits} = parse? input
    List.keepIf edits \edit ->
        isValid edit befores
    |> List.mapTry? findMiddleElement
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

part2 : Str -> Result U64 _
part2 = \input ->
    {befores, edits} = parse? input
    List.keepIf edits \edit ->
        !(isValid edit befores)
    |> List.mapTry? \edit ->
        List.sortWith edit \a, b ->
            aBefores = Dict.get befores a |> Result.withDefault (Set.empty {})
            bBefores = Dict.get befores b |> Result.withDefault (Set.empty {})
            if Set.contains aBefores b then
                LT
            else if Set.contains bBefores a then
                GT
            else
                EQ
        |> findMiddleElement
    |> List.sum
    |> Ok

parse : Str -> Result {befores: Dict U64 (Set U64), edits: List (List U64)} _
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
    befores = List.walk pairs (Dict.empty {}) \dict, pair ->
        Dict.update dict pair.0 \r ->
            when r is
                Err _ -> Set.fromList [pair.1] |> Ok
                Ok set -> Set.insert set pair.1 |> Ok
    Ok {befores, edits}

# Only works on odd length lists
findMiddleElement = \list ->
    List.get list (List.len list // 2)

expect
    result = part1 sampleInput
    result == Ok 143

expect
    result = part2 sampleInput
    result == Ok 123

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
