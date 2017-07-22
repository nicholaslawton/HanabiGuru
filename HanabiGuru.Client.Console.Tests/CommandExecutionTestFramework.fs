module HanabiGuru.Client.Console.Tests.CommandExecutionTestFramework

open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine

let execute commands =
    let execute history = Commands.execute history >> function
        | Error reason -> new AssertionFailedException(sprintf "Cannot perform action: %A" reason) |> raise
        | Ok events -> EventHistory.recordEvents history events
    let applyEvent = GameState.apply GameEvent.apply GameEvent.toEventForPlayer PlayerEvent.apply

    commands
    |> List.fold execute EventHistory.empty
    |> EventHistory.events
    |> List.fold applyEvent GameState.initial
