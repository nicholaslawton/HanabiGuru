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

let pipeline gameUpdated commandFailed inputInvalid inputStream =
    let separateErrors = function
        | Result.Ok result -> Choice1Of2 result
        | Result.Error error -> Choice2Of2 error
    let inputParsingPipeline = Observable.map parseCommand
    let commandExecutionPipeline = 
        Observable.scan (fun (_, history) command ->
            match Commands.execute history command with
            | Result.Ok event -> Result.Ok event |> Some, EventHistory.recordEvent history event
            | Result.Error reasons -> Result.Error reasons |> Some, history)
            (None, EventHistory.empty)
        >> Observable.choose fst
    let eventProcessingPipeline = Observable.scan GameData.processEvent GameData.initial

    inputStream
    |> inputParsingPipeline
    |> Observable.split separateErrors
    |> fun (commands, invalidInput) ->
        commands
        |> commandExecutionPipeline
        |> Observable.split separateErrors
        |> fun (events, failedCommands) ->
            [
                events |> eventProcessingPipeline |> Observable.subscribe gameUpdated
                failedCommands |> Observable.subscribe commandFailed
                invalidInput |> Observable.subscribe inputInvalid
            ]
