app "day1"
        packages {
            pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
        }
        imports [
            pf.Stdout,
            pf.Task.{ Task },
            Common.{ dbge, unwrap },
            "input/1.txt" as input : Str,
        ]
        provides [main] to pf

main : Task {} I32
main = Stdout.line "Part 1: \(part1)\nPart 2: \(part2)"

part1 : Str
part1 = input 
    |> Str.split "\n"
    |> List.map countLine
    |> List.sum
    |> Num.toStr

countLine = \line -> 
    digits = line 
        |> Str.toUtf8
        |> List.keepIf \ch -> 
            ch >= asciiNumberOffset && ch <= asciiNumberOffset + 10
    x = List.first digits |> unwrap |> Num.toU16
    y = List.last digits |> unwrap |> Num.toU16
    (x - asciiNumberOffset) * 10 + (y - asciiNumberOffset)

asciiNumberOffset = 48

part2 : Str
part2 = "_"
