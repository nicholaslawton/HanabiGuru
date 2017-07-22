module HanabiGuru.Client.Console.Tests.AddPlayersIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Client.Console
open HanabiGuru.Engine.Tests

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Players are added to master view`` (PlayerNames names) =
    names
    |> List.map AddPlayer
    |> CommandExecutionTestFramework.execute
    |> fun game -> game.masterView.players
    |> List.map (fun player -> player.identity.name)
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
    |> List.tail =! List.replicate repetitions ([PlayerAlreadyJoined] |> CannotAddPlayer |> Error)
