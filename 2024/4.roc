app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.File

main =
    input = File.readUtf8! "input/4.txt" |> Str.trim
    Stdout.line!
        """
        Part 1: $(Inspect.toStr (part1 input))
        Part 2: $(Inspect.toStr (part2 input))
        """

expect
    result = part1 sampleInput
    result == 14

part1 : Str -> U64
part1 = \input ->
    grid = parse input
    List.walkWithIndex grid 0 \total, row, x ->
        List.walkWithIndex row total \count, elem, y ->
            when elem is
                'X' ->
                    countXmas grid x y
                    |> Result.withDefault 0
                    |> Num.add count

                _ -> count

countXmas : List (List U8), U64, U64 -> Result U64 _
countXmas = \grid, x, y ->
    Ok 1

parse : Str -> List (List U8)
parse = \input ->
    input
    |> Str.toUtf8
    |> List.splitOn '\n'

part2 = \input ->
    "wip"

sampleInput =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """
