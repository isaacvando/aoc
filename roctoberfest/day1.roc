app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
    ]
    provides [main] to pf

main : Task {} I32
main =
    path = "input/1.txt"
    result <- Path.fromStr path |> File.readUtf8 |> Task.attempt
    when result is
        Err _ -> Stdout.line "I wasn't able to read from '\(path)'"
        Ok file -> solve file |> Stdout.line

solve : Str -> Str
solve = \input ->
    input
    |> Str.split "\n\n"
    |> List.map getSum
    |> List.max
    |> Result.withDefault 0
    |> Num.toStr

getSum : Str -> Nat
getSum = \group ->
    group
    |> Str.split "\n"
    |> List.map \s ->
        Str.toNat s |> Result.withDefault 0
    |> List.sum
