module HanabiGuru.Client.Console.Tests.AddPlayersIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine

[<Property>]
let ``Players are added to master view`` (names : string list) =
    let applyEvents =
        let applyEvent = GameState.apply GameEvent.apply GameEvent.toEventForPlayer PlayerEvent.apply
        List.fold applyEvent

    names
    |> List.map AddPlayer
    |> List.map (Commands.execute EventHistory.empty)
    |> List.choose (function
        | Ok events -> Some events
        | Error _ -> None)
    |> List.fold applyEvents GameState.initial
    |> fun game -> game.masterView.players
    |> List.map (fun player -> player.name)
    |> List.sort =! List.sort names

[<Property>]
let ``Adding the same player to the game repeatedly returns errors`` (name : string) (PositiveInt repetitions) =
    name
    |> List.replicate (repetitions + 1)
    |> List.map AddPlayer
    |> List.scan
        (fun (_, history) command ->
            match Commands.execute history command with
            | Ok events -> Ok events |> Some, List.fold EventHistory.recordEvent history events
            | Error reasons -> Error reasons |> Some, history)
        (None, EventHistory.empty)
    |> List.choose fst
    |> List.tail =! List.replicate repetitions ([PlayerAlreadyJoined] |> Error)
