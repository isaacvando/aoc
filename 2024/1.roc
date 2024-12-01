app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br" }

import pf.Stdout
import pf.File
import pf.Utc

main =
    input = File.readUtf8! "input/1.txt"
    part1Output = run! Part1 input
    part2Output = run! Part2 input
    Stdout.line!
        """
        Part 1: $(part1Output)
        Part 2: $(part2Output)
        """

run = \part, input ->
    start = Utc.now! {}
    result =
        when part is
            Part1 -> part1 input
            Part2 -> part2 input
    end = Utc.now! {}
    Task.ok "$(Inspect.toStr result) in $(Utc.deltaAsMillis start end |> Num.toStr)ms"

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
    |> List.walk { left: [], right: [], parity: Bool.true } \{left, right, parity}, elem ->
        if parity then
            {left: List.append left elem, right, parity: !parity}
        else
            {left, right: List.append right elem, parity: !parity}

unwrap = \r ->
    when r is
        Err e -> crash "Unwrap failed: $(Inspect.toStr e)"
        Ok val -> val
