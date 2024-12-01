app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stderr
import pf.Http
import pf.Arg
import pf.File
import pf.Env

main =
    args = Arg.list! {}
    when args is
        [] | [_] -> Stderr.line "Please pass in the day you would like to set up"
        [_, day, ..] ->
            session = Env.var! "AOC_SESSION"
            _ =
                Http.defaultRequest
                |> &url "https://adventofcode.com/2024/day/$(day)/input"
                |> &headers [{ key: "Cookie", value: "session=$(session)" }]
                |> Http.send!
                |> .body
                |> File.writeBytes! "input/$(day).txt"

            File.readUtf8! "template.roc"
            |> Str.replaceEach "{{DAY}}" day
            |> File.writeUtf8! "$(day).roc"
