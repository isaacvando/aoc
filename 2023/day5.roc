app "day5"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core,
        parser.String,
        "input/5.txt" as file : Str,
        "input/5ex.txt" as example : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    # Stdout.line "Part 1: \(part1 file)\nPart 2: \(part2 file)"
    dbg part1 example
    Task.ok {}

Map : List {dest: Nat, source: Nat, len: Nat}

part1 = \input ->
    almanac = parse input
        |> unwrap
    tables = almanac.maps 
        |> List.map mapToTable
    almanac.seeds
    |> List.map \seed -> 
        traverse tables seed
    # |> List.min
    # |> unwrap
    
mapToTable : Map -> Dict Nat Nat
mapToTable = \map ->
    List.walk map (Dict.empty {}) \dict, {dest, source, len} -> 
        inputs = List.range {start: At source, end: Length len}
        outputs = List.range {start: At dest, end: Length len}
        ios = List.map2 inputs outputs \i, o -> (i,o)
        List.walk ios dict \state, (i, o) -> 
            Dict.insert state i o

traverse = \tables, seed ->
    List.walk tables seed \state, table -> 
        when Dict.get table state is
            Ok output -> output
            _ -> state

part2 = \input -> "-"

parse = \input -> 
    numbers = Core.sepBy1 String.digits (String.scalar ' ')

    seeds = Core.const (\x -> x)
        |> Core.skip (String.string "seeds: ")
        |> Core.keep numbers

    line = Core.const (\_ -> {})
        |> Core.skip (Core.chompUntil '\n')
        |> Core.keep (String.scalar '\n')

    range = 
        Core.const (\dest -> \source -> \len -> {dest, source, len})
        |> Core.keep String.digits
        |> Core.skip (String.scalar ' ')
        |> Core.keep String.digits
        |> Core.skip (String.scalar ' ')
        |> Core.keep String.digits

    map = Core.const (\m -> m)
        |> Core.skip line
        |> Core.skip line
        |> Core.keep (Core.sepBy range (String.scalar '\n'))

    almanac = Core.const (\s -> \ms -> {seeds: s, maps: ms})
        |> Core.keep seeds
        |> Core.skip line
        |> Core.keep (Core.sepBy map line)

    String.parseStr almanac input

unwrap = \r -> 
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x -> 
    dbg x
    x
