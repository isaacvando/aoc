app "day6"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ Parser },
        parser.String,
        "input/6.txt" as file : Str,
        "input/6ex.txt" as example : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    Stdout.line "Part 1: \(part1 file)\nPart 2: \(part2 file)"

part1 = \input ->
    input
    |> parse
    |> List.map countWays
    |> List.product
    |> Num.toStr

countWays = \{ time, dist } ->
    List.range { start: At 0, end: Length time }
    |> List.countIf \buttonTime ->
        distance buttonTime time > dist

distance = \buttonTime, totalTime ->
    buttonTime * (totalTime - buttonTime)

part2 = \input ->
    parse2 input
    |> countWays
    |> Num.toStr

parse = \input ->
    eatNonDigits = Core.chompWhile \c ->
        !(isDigit c || c == '\n')

    numbers =
        Core.const (\x -> x)
        |> Core.skip eatNonDigits
        |> Core.keep (Core.sepBy1 String.digits eatNonDigits)

    collectPage = \t -> \d ->
            List.map2 t d \x, y ->
                { time: x, dist: y }

    page =
        Core.const collectPage
        |> Core.keep numbers
        |> Core.skip (String.scalar '\n')
        |> Core.keep numbers

    String.parseStr page input
    |> unwrap

parse2 = \input ->
    getNat = \line ->
        Str.toUtf8 line
        |> List.keepIf isDigit
        |> Str.fromUtf8
        |> unwrap
        |> Str.toNat
        |> unwrap

    when Str.split input "\n" is
        [one, two] -> { time: getNat one, dist: getNat two }
        _ -> crash "invalid input"

unwrap = \r ->
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

isDigit = \c ->
    List.contains ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] c
