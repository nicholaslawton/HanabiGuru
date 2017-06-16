module HanabiGuru.Client.Console.Tests.CommandExecutionTestFramework

open HanabiGuru.Client.Console
open HanabiGuru.Engine

let execute commands =
    let applyEvents =
        let applyEvent = GameState.apply GameEvent.apply GameEvent.toEventForPlayer PlayerEvent.apply
        List.fold applyEvent

    commands
    |> List.map (Commands.execute EventHistory.empty)
    |> List.choose (function
        | Ok events -> Some events
        | Error _ -> None)
    |> List.fold applyEvents GameState.initial
