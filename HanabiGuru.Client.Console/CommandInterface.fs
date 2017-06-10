module HanabiGuru.Client.Console.CommandInterface

open FParsec

let processCommands getInput pipeline =
    let events = new Event<_> ()
    let rec processNextInput () =
        getInput ()
        |> Option.iter (fun input ->
            events.Trigger input
            processNextInput () |> ignore)
    use subscription = events.Publish |> pipeline
    processNextInput ()

let parseCommand input =
    let addPlayer =
        let prefix = skipString "add" >>. spaces >>. skipString "player" >>. spaces <?> "command"
        let name = many1Chars anyChar <?> "player name"
        prefix >>. name
    match run addPlayer input with
    | Success (name, _, _) -> AddPlayer name |> Result.Ok
    | Failure (errorMessage, _, _) -> Result.Error (sprintf "%A" errorMessage)
