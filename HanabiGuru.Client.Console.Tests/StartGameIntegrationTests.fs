module HanabiGuru.Client.Console.Tests.StartGameIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Client.Console
open HanabiGuru.Engine.Tests

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``After starting the game, all players have cards in their hands`` (CompletePlayerNames names) =
    let commands = (List.map AddPlayer names) @ [StartGame]
    let game = CommandExecutionTestFramework.execute commands

    let hasEmptyHand player = List.isEmpty player.hand

    [
        game.masterView.players |> List.filter hasEmptyHand =! []
            |@ "No empty hands in master view"
        game.playerViews |> List.filter (fun view -> hasEmptyHand view.self) =! []
            |@ "No empty hands for self"
        game.playerViews |> List.filter (fun view -> view.otherPlayers |> List.exists hasEmptyHand) =! []
            |@ "No empty hands for other players"
    ]
