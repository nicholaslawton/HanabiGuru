module HanabiGuru.Client.Console.Tests.AddPlayersIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Client.Console

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Players can be added to game`` (Names names) =
    names
    |> Set.toList
    |> List.map (sprintf "add player %s")
    |> CommandProcessingTestFramework.processInput
    |> GameState.players =! Set.map PlayerIdentity.create names

[<Property>]
let ``Adding the same player to the game repeatedly returns errors`` (name : string) (PositiveInt repetitions) =
    name
    |> List.replicate (1 + repetitions)
    |> List.map AddPlayer
    |> List.scan
        (fun (_, history) command ->
            match Commands.execute history command with
            | Ok newHistory -> Ok newHistory |> Some, newHistory
            | Error reasons -> Error reasons |> Some, history)
        (None, EventHistory.empty)
    |> List.choose fst
    |> List.tail =! List.replicate repetitions ([PlayerAlreadyJoined] |> CannotAddPlayer |> Error)
