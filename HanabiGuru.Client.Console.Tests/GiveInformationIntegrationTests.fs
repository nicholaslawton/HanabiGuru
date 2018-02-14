module HanabiGuru.Client.Console.Tests.GiveInformationIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Giving information reduces the number of candidates`` (ValidNames names) =
    let game = CommandProcessingTestFramework.startGame names
    let candidates game =
        GameState.players game
        |> List.map (fun player ->
            let view = GameState.playerView player game
            PlayerView.hand view |> List.map (PlayerView.CardIdentity.deduce view))
        |> List.concat
        |> List.concat
    CommandProcessingTestFramework.giveInformation game
    |> Result.map (candidates >> List.length) <! (candidates game |> List.length |> Ok)
