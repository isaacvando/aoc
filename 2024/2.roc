app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br" }

import pf.Stdout
import pf.File

main =
    input = File.readUtf8! "input/2.txt"
    Stdout.line!
        """
        Part 1: $(Inspect.toStr (part1 input))
        Part 2: $(Inspect.toStr (part2 input))
        """
sampleInput =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """

expect
    result = part1 sampleInput
    result == Ok 2

part1 = \input ->
    reports = try parse input
    List.countIf reports isSafe |> Ok

isSafe = \report ->
    help = \direction, remaining ->
        when remaining is
            [] | [_] ->
                Bool.true

            [x, y, ..] ->
                when direction is
                    Tbd -> if x < y then help Increasing remaining else help Decreasing remaining
                    Increasing ->
                        if y - x < 1 || y - x > 3 then
                            Bool.false
                            else

                        help Increasing (List.dropFirst remaining 1)

                    Decreasing ->
                        if x - y < 1 || x - y > 3 then
                            Bool.false
                            else

                        help Decreasing (List.dropFirst remaining 1)

    help Tbd report

isSafe2 = \report ->
    help = \index ->
        if index == List.len report then
            Bool.false
        else if List.dropAt report index |> isSafe then
            Bool.true
        else
            help (index + 1)
    isSafe report || help 0

part2 = \input ->
    reports = try parse input
    List.countIf reports isSafe2 |> Ok

expect
    result = part2 sampleInput
    result == Ok 4

parse : Str -> Result (List (List I32)) _
parse = \input ->
    input
    |> Str.trim
    |> Str.splitOn "\n"
    |> List.mapTry \line ->
        Str.splitOn line " "
        |> List.mapTry Str.toI32

unwrap = \r ->
    when r is
        Err e -> crash "Unwrap failed: $(Inspect.toStr e)"
        Ok val -> val
