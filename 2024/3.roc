app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.File

main =
    input = File.readUtf8! "input/3.txt" |> Str.trim |> Str.toUtf8
    Stdout.line!
        """
        Part 1: $(Inspect.toStr (part1 input))
        Part 2: $(Inspect.toStr (part2 input))
        """

part1 = \input ->
    parse input Bool.false
    |> List.map \{ x, y } -> x * y
    |> List.sum

expect
    result = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" |> Str.toUtf8 |> part1
    result == 161

part2 = \input ->
    parse input Bool.true
    |> List.map \{ x, y } -> x * y
    |> List.sum

expect
    result = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" |> Str.toUtf8 |> part2
    result == 48

parse : List U8, Bool -> List { x : U64, y : U64 }
parse = \input, respectDoCommands ->
    help = \remaining, muls ->
        when remaining is
            [] -> muls
            ['d', 'o', 'n', '\'', 't', '(', ')', .. as rest] if respectDoCommands ->
                help (dropUntilDo rest) muls

            ['m', 'u', 'l', '(', .. as rest] ->
                when parseNumber rest ',' is
                    Err _ -> help rest muls
                    Ok { val: firstNum, bytes: firstBytes } ->
                        when parseNumber firstBytes ')' is
                            Err _ -> help firstBytes muls
                            Ok { val: secondNum, bytes: secondBytes } ->
                                help secondBytes (List.append muls { x: firstNum, y: secondNum })

            [_, .. as rest] -> help rest muls

    help input []

dropUntilDo : List U8 -> List U8
dropUntilDo = \input ->
    when input is
        [] -> []
        ['d', 'o', '(', ')', .. as rest] -> rest
        [_, .. as rest] -> dropUntilDo rest

parseNumber : List U8, U8 -> Result { val : U64, bytes : List U8 } _
parseNumber = \input, terminator ->
    help = \remaining, digits ->
        when remaining is
            [x, .. as rest] if x == terminator ->
                num =
                    Str.fromUtf8? digits
                        |> Str.toU64?
                Ok { val: num, bytes: rest }

            [x, .. as rest] if '0' <= x && x <= '9' ->
                help rest (List.append digits x)

            _ -> Err (UnableToParse remaining)
    help input []
