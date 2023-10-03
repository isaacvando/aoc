app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "input/2.txt" as file : Str,
    ]
    provides [main] to pf

main : Task {} I32
main =
    Stdout.line solve1

solve1 : Str
solve1 =
    file
    |> Str.split "\n"
    |> List.map round
    |> List.sum
    |> Num.toStr

round : Str -> Nat
round = \r ->
    r
    |> Str.split " "
    |> toPair
    |> score

Rps : [Roc, Paper, Scissors]

toPair : List Str -> (Rps, Rps)
toPair = \letters ->
    when letters is
        [m, n] -> (toRps m, toRps n)
        _ -> (Roc, Roc) # This won't happen

toRps : Str -> Rps
toRps = \s ->
    when s is
        "A" -> Roc
        "B" -> Paper
        "C" -> Scissors
        "X" -> Roc
        "Y" -> Paper
        "Z" -> Scissors
        _ -> Roc # this won't happen

score : (Rps, Rps) -> Nat
score = \(l, r) ->
    base =
        when r is
            Roc -> 1
            Paper -> 2
            Scissors -> 3
    match =
        when (l, r) is
            (Roc, Scissors) -> 0
            (Roc, Roc) -> 3
            (Roc, Paper) -> 6
            (Paper, Roc) -> 0
            (Paper, Paper) -> 3
            (Paper, Scissors) -> 6
            (Scissors, Paper) -> 0
            (Scissors, Scissors) -> 3
            (Scissors, Roc) -> 6
    base + match
