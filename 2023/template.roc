app "day2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "input/2.txt" as file : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    Stdout.line "Part 1: \(part1 file)\nPart 2: \(part2 file)"

part1 = \input -> "-"

part2 = \input -> "-"

unwrap = \r -> 
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x -> 
    dbg x
    x
