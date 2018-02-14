module HanabiGuru.Client.Console.Tests.AddPlayersIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Client.Console

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Players can be added to game`` (ValidNames names) =
    let names = Set.toList names
    names
    |> List.map (sprintf "add player %s")
    |> CommandProcessingTestFramework.processInput
    |> GameState.players
    |> List.sort =! (names |> List.map PlayerIdentity.create |> List.sort)

[<Property>]
let ``Adding the same player to the game repeatedly returns errors`` (name : string) (PositiveInt repetitions) =
    name
    |> List.replicate (1 + repetitions)
    |> List.map AddPlayer
    |> List.scan
        (fun (_, history) command ->
            match Command.execute history command with
            | Ok newHistory -> Ok newHistory |> Some, newHistory
            | Error reasons -> Error reasons |> Some, history)
        (None, EventHistory.empty)
    |> List.choose fst
    |> List.tail =! List.replicate repetitions ([PlayerAlreadyJoined] |> CannotAddPlayer |> ExecutionFailure |> Error)
