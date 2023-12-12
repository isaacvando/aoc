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
        "input/7.txt" as file : Str,
        "input/7ex.txt" as example : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    # Stdout.line "Part 1: \(part1 file)\nPart 2: \(part2 file)"
    dbg
        part1 file

    Task.ok {}

part1 = \input ->
    parse input
    |> List.sortWith \x, y ->
        compareHands x.hand y.hand
    |> List.reverse
    |> List.walkWithIndex 0 \total, {hand, bid}, index -> 
        dbg (index + 1, bid)
        total + bid * (index + 1)

part2 = \input -> "_"

unwrap = \r ->
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x ->
    dbg
        x

    x

parse = \input ->
    cardParsers = List.map2 cards cardSymbols \c, s ->
        String.scalar s |> Core.map \_ -> c

    card = Core.oneOf cardParsers

    hand = Core.many card

    line =
        Core.const (\h -> \b -> { hand: h, bid: b })
        |> Core.keep hand
        |> Core.skip (String.scalar ' ')
        |> Core.keep (String.digits)

    lines = Core.sepBy1 line (String.scalar '\n')

    String.parseStr lines input
    |> unwrap

compareHands = \h1, h2 ->
    r1 = type h1
    r2 = type h2

    if
        r1 == r2
    then
        compareSameTypeHands h1 h2
    else if
        r1 < r2
    then
        LT
    else
        GT

compareSameTypeHands = \h1, h2 ->
    when (h1, h2) is
        ([], []) -> EQ
        ([x, .. as xs], [y, .. as ys]) ->
            comp = compareCards x y
            when comp is
                EQ -> compareSameTypeHands xs ys
                _ -> comp

        _ -> crash "invalid hand"

compareCards = \x, y ->
    r1 = Dict.get cardRanks x |> unwrap
    r2 = Dict.get cardRanks y |> unwrap

    if
        r1 == r2
    then
        EQ
    else if
        r1 < r2
    then
        LT
    else
        GT

cardRanks =
    ranks = List.range { start: At 0, end: Length (List.len cards) }
    List.map2 cards ranks \c, r ->
        (c, r)
    |> Dict.fromList

type = \hand ->
    typePredicates
    |> List.findFirstIndex \pred ->
        pred hand
    |> unwrap

typePredicates = [
    nOfAKind 5,
    nOfAKind 4,
    fullHouse,
    nOfAKind 3,
    twoPairs,
    nOfAKind 2,
    nOfAKind 1,
]

twoPairs = \hand ->
    twos =
        getOccurances hand
        |> List.keepIf \(_, count) ->
            count == 2

    List.len twos == 2

fullHouse = \hand ->
    when getOccurances hand is
        [(_, count1), (_, count2)] -> Num.abs (count1 - count2) == 1
        _ -> Bool.false

expect fullHouse [Ace, Ace, Queen, Ace, Queen]

nOfAKind = \n -> \hand ->
        getOccurances hand
        |> List.any \(card, count) ->
            count == n

expect (nOfAKind 5) [Ace, Ace, Ace, Ace, Ace]
expect (nOfAKind 4) [Ace, Ace, Ace, Queen, Ace]
expect (nOfAKind 3) [Ace, Jack, Ace, Queen, Ace]

getOccurances = \hand ->
    List.walk hand (Dict.empty {}) \dict, elem ->
        Dict.update dict elem \val ->
            when val is
                Present n -> Present (n + 1)
                Missing -> Present 1
    |> Dict.toList

ofAKind = \distinct -> \hand ->
        set = Set.fromList hand
        Set.len set == distinct

Card : [
    Ace,
    King,
    Queen,
    Jack,
    Ten,
    Nine,
    Eight,
    Seven,
    Six,
    Five,
    Four,
    Three,
    Two,
    One,
]

cards = [
    Ace,
    King,
    Queen,
    Jack,
    Ten,
    Nine,
    Eight,
    Seven,
    Six,
    Five,
    Four,
    Three,
    Two,
    One,
]

cardSymbols = [
    'A',
    'K',
    'Q',
    'J',
    'T',
    '9',
    '8',
    '7',
    '6',
    '5',
    '4',
    '3',
    '2',
    '1',
]
