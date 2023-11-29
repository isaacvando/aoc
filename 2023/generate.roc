# generate a roc file for a given day
# TODO: authenticate through aoc and downlod the input
app "generate"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        pf.Arg
    ]
    provides [main] to pf

main : Task {} I32
main = 
    args <- Arg.list |> Task.await
    when args is
        [_, day, ..] if Str.toU8 day |> Result.isOk -> 
            Path.fromStr "day\(day).roc" 
                |> File.writeUtf8 (template day)
                |> Task.await \_ -> Stdout.line "Wrote file to day\(day).roc"
                |> Task.onErr \_ -> Stdout.line "Uh oh, there was an writing the file"
        _ -> Stdout.line "Please pass the day"

template = \day -> 
    """
    app "day\(day)"
        packages {
            pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br",
            parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
        }
        imports [
            pf.Stdout,
            pf.Task.{ Task },
            "input/\(day).txt" as input : Str,
        ]
        provides [main] to pf

    main : Task {} I32
    main =
        _ <- "Part 1: "
            |> Str.concat part1
            |> Stdout.line
            |> Task.await
        "Part 2: "
        |> Str.concat part2
        |> Stdout.line

    part1 : Str
    part1 = "_"

    part2 : Str
    part2 = "_"
    """
