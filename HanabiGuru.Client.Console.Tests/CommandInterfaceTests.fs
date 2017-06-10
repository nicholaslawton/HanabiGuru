module HanabiGuru.Client.Console.Tests.CommandInterfaceTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console

[<Property>]
let ``Command processing processes commands received and then terminates`` (commands : string list) =
    let mutable commandsQueue = commands
    let mutable commandsProcessed = []

    let processCommand command = commandsProcessed <- command :: commandsProcessed
    let pipeline stream = stream |> Observable.subscribe processCommand

    let getInput () =
        match commandsQueue with
        | command :: remainingCommands ->
            commandsQueue <- remainingCommands
            Some command
        | [] -> None

    CommandInterface.processCommands getInput pipeline

    commandsQueue =! []
    List.rev commandsProcessed =! commands
