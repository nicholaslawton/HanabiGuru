module HanabiGuru.Client.Console.Tests.AddPlayersIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine
open HanabiGuru.Engine.Tests

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Players are added to master view`` (ValidPlayerNames names) =
    names
    |> List.map AddPlayer
    |> List.map (Commands.execute EventHistory.empty)
    |> List.choose (function
        | Ok event -> Some event
        | Error _ -> None)
    |> List.fold GameData.apply GameData.initial
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
            | Ok event -> Ok event |> Some, EventHistory.recordEvent history event
            | Error reasons -> Error reasons |> Some, history)
        (None, EventHistory.empty)
    |> List.choose fst
    |> List.tail =! List.replicate repetitions ([PlayerAlreadyJoined] |> Error)
