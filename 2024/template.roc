app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br" }

import pf.Stdout
import pf.File

main =
    input = File.readUtf8! "input/{{DAY}}.txt" |> Str.trim
    Stdout.line!
        """
        Part 1: $(Inspect.toStr (part1 input))
        Part 2: $(Inspect.toStr (part2 input))
        """

part1 = \input ->
    "wip"

part2 = \input ->
    "wip"
