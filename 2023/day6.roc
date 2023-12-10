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

    dbg
        part1 (parse file)

    Task.ok {}

part1 = \input ->
    input
    |> List.map countWays
    |> List.product

countWays = \{ time, dist } ->
    List.range { start: At 0, end: Length time }
    |> List.countIf \buttonTime ->
        distance buttonTime time > dist

distance = \buttonTime, totalTime ->
    buttonTime * (totalTime - buttonTime)

part2 = \input -> "_"

parse = \input ->
    eatNonDigits = Core.chompWhile \c ->
        !(List.contains ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '\n'] c)

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

unwrap = \r ->
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x ->
    dbg
        x

    x
