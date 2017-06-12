module HanabiGuru.Client.Console.CommandInterface

open FParsec
open System
open HanabiGuru.Engine

let processCommands getInput pipeline =
    let events = new Event<_> ()
    let rec processNextInput () =
        getInput ()
        |> Option.iter (fun input ->
            events.Trigger input
            processNextInput () |> ignore)
    let subscriptions = events.Publish |> pipeline
    processNextInput ()
    subscriptions |> List.iter (fun (subscription : IDisposable) -> subscription.Dispose())

let parseCommand input =
    let addPlayer =
        let prefix = skipString "add" >>. spaces >>. skipString "player" >>. spaces <?> "command"
        let name = many1Chars anyChar <?> "player name"
        prefix >>. name
    match run addPlayer input with
    | Success (name, _, _) -> AddPlayer name |> Result.Ok
    | Failure (errorMessage, _, _) -> Result.Error (sprintf "%A" errorMessage)

let pipeline handleFailedCommand handleInvalidInput inputStream =
    inputStream
    |> Observable.map (parseCommand)
    |> Observable.split (function
        | Result.Ok command -> Choice1Of2 command
        | Result.Error message -> Choice2Of2 message)
    |> fun (commands, errors) ->
        [
            errors |> Observable.subscribe handleInvalidInput

            commands
            |> Observable.scan (Commands.execute handleFailedCommand) EventHistory.empty
            |> Observable.map (EventHistory.allEvents)
            |> Observable.map (List.fold GameData.processEvent GameData.initial)
            |> Observable.subscribe (printfn "%A")
        ]
