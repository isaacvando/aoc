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
    dbg solve file Bool.true

    Task.ok {}

solve = \input, wildJokers ->
    parse input
    |> List.sortWith \x, y ->
        compareHands wildJokers x.hand y.hand
    |> List.reverse # |> List.map Inspect.toStr
    # |> Str.joinWith "\n"
    |> List.walkWithIndex 0 \total, { hand, bid }, index ->
        total + bid * (index + 1)

unwrap = \r ->
    when r is
        Err _ -> crash "unwrap encountered an Err"
        Ok val -> val

dbge = \x ->
    dbg x

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

compareHands = \wildJokers, h1, h2 ->
    subJokers = \hand ->
        jokers = List.countIf hand \elem -> elem == Jack
        mostCommon = getMostCommon hand

        List.dropIf hand \elem ->
            elem == Jack
        |> List.concat (List.repeat mostCommon jokers)

    r1 = if wildJokers then type (subJokers h1) else type h1
    r2 = if wildJokers then type (subJokers h2) else type h2

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

getMostCommon : Hand -> Card
getMostCommon = \hand ->
    getOccurances hand
    |> Dict.toList
    |> List.sortWith \(_, x), (_, y) -> Num.compare x y
    |> List.last
    |> unwrap
    |> .0

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

    Num.compare r1 r2

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

expect type [] == 0
expect type [Ace] == 0
expect type [Ace, Ace] == 0
expect type [Ace, Queen, Queen, Ace] == 2
expect type [Ace, Queen] == 1
expect type [Ace, Queen, Jack] == 3
expect type [Ace, Queen, Jack, Ten] == 5
expect type [Ace, Queen, Jack, Ten, Nine] == 6

typePredicates : List (Hand -> Bool)
typePredicates = [
    nOfAKind 5,
    nOfAKind 4,
    fullHouse,
    nOfAKind 3,
    twoPairs,
    nOfAKind 2,
    nOfAKind 1,
]

twoPairs : Hand -> Bool
twoPairs = \hand ->
    numberOfTwos =
        getOccurances hand
        |> Dict.values
        |> List.countIf \count ->
            count == 2

    when List.len hand is
        5 -> numberOfTwos == 2
        4 -> numberOfTwos == 1
        _ -> Bool.true

expect twoPairs []
expect twoPairs [Ace]
expect twoPairs [Ace, Ace]
expect twoPairs [Ace, King]
expect twoPairs [Ace, Ace, Ace]
expect !(twoPairs [Ace, King, Queen, Jack])
expect twoPairs [Ace, Ace, Queen, Queen, Jack]
expect twoPairs [Ace, Queen, Jack]
expect !(twoPairs [Ace, Queen, Ten, Four, Three])

fullHouse : Hand -> Bool
fullHouse = \hand ->
    occs = getOccurances hand |> Dict.values
    when List.len hand is
        5 -> List.contains occs 2 && List.contains occs 3
        4 -> List.contains occs 3 || occs == [2, 2]
        3 -> occs == [3] || List.contains occs 2
        _ -> Bool.true

expect fullHouse [Ace, Ace, Queen, Ace, Queen]
expect !(fullHouse [Ace, Queen, Queen, Queen, Ten])
expect !(fullHouse [One, Two, Three, Ten, Queen])
expect fullHouse []
expect fullHouse [Ace]
expect fullHouse [Ace, Ace]
expect fullHouse [Ace, Queen]
expect fullHouse [Ace, Ace, Queen]
expect !(fullHouse [One, Two, Three])
expect fullHouse [One, Two, Two, One]
expect !(fullHouse [One, Two, Two, Ace])

nOfAKind : Nat -> (Hand -> Bool)
nOfAKind = \n -> \hand ->
        occs = getOccurances hand |> Dict.values
        occs
        == []
        || List.any occs \count ->
            count + (5 - List.len hand) >= n

expect (nOfAKind 5) [Ace, Ace, Ace, Ace, Ace]
expect (nOfAKind 4) [Ace, Ace, Ace, Queen, Ace]
expect (nOfAKind 3) [Ace, Jack, Ace, Queen, Ace]
expect (nOfAKind 3) [Ace, Ace, Queen, King]
expect (nOfAKind 1) [Ace]
expect (nOfAKind 2) [Ace]
expect (nOfAKind 2) []
expect (nOfAKind 4) [Ace, Ace, Ace, Queen]

getOccurances = \hand ->
    List.walk hand (Dict.empty {}) \dict, elem ->
        Dict.update dict elem \val ->
            when val is
                Present n -> Present (n + 1)
                Missing -> Present 1

findBestHand = \hand ->
    hand

computeAllPossible = \done, remaining ->
    (d, r) = List.walk remaining ([], []) \(x, y), hand ->
        when List.findFirstIndex hand \elem -> elem == Jack is
            Ok index ->
                combinations = List.map withoutJoker \c ->
                    List.set hand index c
                (x, List.concat y combinations)

            _ -> (List.append x hand, y)

    when r is
        [] -> d
        _ -> computeAllPossible d r

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

Hand : List Card

withoutJoker = [
    Ace,
    King,
    Queen,
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
