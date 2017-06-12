module HanabiGuru.Client.Console.Tests.AddPlayersIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine
open HanabiGuru.Engine.Tests

[<Property>]
let ``Adding the same player to the game repeatedly reports errors`` (name : string) (PositiveInt repetitions) =
    let mutable errors = []

    let handleError error = errors <- error :: errors

    name
    |> List.replicate (repetitions + 1)
    |> List.map AddPlayer
    |> List.fold (Commands.execute handleError) EventHistory.empty
    |> ignore

    errors =! List.replicate repetitions [PlayerAlreadyJoined]

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Players are added to game state`` (ValidPlayerNames names) =
    names
    |> List.map AddPlayer
    |> List.fold (Commands.execute ignore) EventHistory.empty
    |> EventHistory.allEvents
    |> List.fold GameData.processEvent GameData.initial
    |> fun game -> game.state.players
    |> List.map (fun player -> player.name)
    |> List.sort =! List.sort names
