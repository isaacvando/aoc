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
    result == 18

part1 : Str -> U64
part1 = \input ->
    grid = parse input
    List.mapWithIndex grid \row, y ->
        List.mapWithIndex row \_, x ->
            countXmas grid { x: Num.toI32 x, y: Num.toI32 y }
        |> List.sum
    |> List.sum

countXmas : List (List U8), { x : I32, y : I32 } -> U64
countXmas = \grid, coord ->
    List.map [-1, 0, 1] \x ->
        List.map [-1, 0, 1] \y ->
            { x, y }
    |> List.join
    |> List.map \{ x, y } ->
        List.map [0, 1, 2, 3] \coeff ->
            { x: coord.x + coeff * x, y: coord.y + coeff * y }
    |> List.countIf \indices ->
        letters =
            List.mapTry indices \index ->
                get index grid
            |> Result.withDefault []
        letters == ['X', 'M', 'A', 'S']

get = \{x, y}, grid ->
    xu = Num.toU64Checked? x
    yu = Num.toU64Checked? y
    List.get? grid yu |> List.get xu

expect
    result = part2 sampleInput
    result == 9

part2 = \input ->
    grid = parse input
    List.mapWithIndex grid \row, y ->
        List.mapWithIndex row \letter, x ->
            if letter == 'A' && isX grid { x: Num.toI32 x, y: Num.toI32 y } then 1 else 0
        |> List.sum
    |> List.sum

isX : List (List U8), { x : I32, y : I32 } -> Bool
isX = \grid, coord ->
    letters =
        [
            { x: coord.x - 1, y: coord.y - 1 },
            { x: coord.x + 1, y: coord.y - 1 },
            { x: coord.x + 1, y: coord.y + 1 },
            { x: coord.x - 1, y: coord.y + 1 },
        ]
        |> List.mapTry \index -> get index grid
        |> Result.withDefault []

    List.any [['M', 'M', 'S', 'S'], ['M', 'S', 'S', 'M'], ['S', 'S', 'M', 'M'], ['S', 'M', 'M', 'S']] \order ->
        order == letters

parse : Str -> List (List U8)
parse = \input ->
    input
    |> Str.toUtf8
    |> List.splitOn '\n'

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
