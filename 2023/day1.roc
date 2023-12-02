app "day1"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "input/1.txt" as input : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    Stdout.line "Part 1: \(part1)\nPart 2: \(part2)"

part1 : Str
part1 =
    input
    |> Str.split "\n"
    |> List.map countLine
    |> List.sum
    |> Num.toStr

countLine = \line ->
    digits =
        line
        |> Str.toUtf8
        |> List.keepIf \ch ->
            ch >= asciiNumberOffset && ch <= asciiNumberOffset + 10
    x = List.first digits |> unwrap |> Num.toU16
    y = List.last digits |> unwrap |> Num.toU16
    (x - asciiNumberOffset) * 10 + (y - asciiNumberOffset)

asciiNumberOffset = 48

part2 : Str
part2 =
    input
    |> Str.split "\n"
    |> List.map countLineWithWords
    |> List.sum
    |> Num.toStr

countLineWithWords = \line ->
    digits = buildList [] (Str.toUtf8 line)
    x = List.first digits |> unwrap
    y = List.last digits |> unwrap
    x * 10 + y

buildList : List U16, List U8 -> List U16
buildList = \state, line ->
    when line is
        [] -> state
        [_, .. as rest] ->
            when getFirstNumber line is
                Err _ -> buildList state rest
                Ok num -> buildList (List.append state num) rest

getFirstNumber = \line ->
    numbers
    |> List.findFirst \(name, _) ->
        line |> List.startsWith name
    |> Result.map \(name, value) ->
        value

numbers : List (List U8, U16)
numbers =
    [
        ("0", 0),
        ("1", 1),
        ("2", 2),
        ("3", 3),
        ("4", 4),
        ("5", 5),
        ("6", 6),
        ("7", 7),
        ("8", 8),
        ("9", 9),
        ("zero", 0),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ]
    |> List.map \(str, val) -> (Str.toUtf8 str, val)

unwrap = \r -> 
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val
