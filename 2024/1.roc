app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br" }

import pf.Stdout
import pf.File

main =
    input = File.readUtf8! "input/1.txt"
    Stdout.line!
        """
        Part 1: $(Inspect.toStr (part1 input))
        Part 2: $(Inspect.toStr (part2 input))
        """

part1 = \input ->
    { left, right } = parse input
    List.map2 (List.sortAsc left) (List.sortAsc right) \l, r ->
        Num.abs (l - r)
    |> List.sum

part2 = \input ->
    { left, right } = parse input
    occurances =
        List.walk right (Dict.empty {}) \state, elem ->
            Dict.update state elem \r ->
                when r is
                    Ok count -> Ok (count + 1)
                    Err _ -> Ok 1
    List.map left \elem ->
        Dict.get occurances elem
        |> Result.withDefault 0
        |> Num.mul elem
    |> List.sum

parse = \input ->
    input
    |> Str.trim
    |> Str.replaceEach "   " "\n"
    |> Str.splitOn "\n"
    |> List.map \str ->
        Str.toI32 str |> unwrap
    |> List.walk { left: [], right: [], parity: Bool.true } \state, elem ->
        if state.parity then
            { state & left: List.append state.left elem, parity: !state.parity }
        else
            { state & right: List.append state.right elem, parity: !state.parity }

unwrap = \r ->
    when r is
        Err e -> crash "Unwrap failed: $(Inspect.toStr e)"
        Ok val -> val
