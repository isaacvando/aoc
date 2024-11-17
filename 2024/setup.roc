app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br" }

import pf.Stdout
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
            # session = Env.var! "AOC_SESSION"
            # response =
            #    Http.defaultRequest
            #        |> &url "https://adventofcode.com/2023/day/$(day)/input"
            #        |> &headers [{ key: "Cookie", value: "session=$(session)" }]
            #        |> Http.send!
            response = { body: [] }

            File.writeBytes! "input/$(day).txt" response.body
            template = File.readUtf8! "template.roc"
            File.writeUtf8! "$(day).roc" (Str.replaceEach template "{{DAY}}" day)
