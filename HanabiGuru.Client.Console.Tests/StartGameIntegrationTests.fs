module HanabiGuru.Client.Console.Tests.StartGameIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Client.Console
open HanabiGuru.Engine.Tests

let private startGame names =
    StartGame :: List.map AddPlayer names
    |> List.rev
    |> CommandExecutionTestFramework.execute

let private hasEmptyHand player = List.isEmpty player.hand

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``After starting the game, all players have cards in their hands in the master view`` (CompletePlayerNames names) =
    startGame names |> fun game -> game.masterView.players |> List.filter hasEmptyHand =! []

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``After starting the game, all players have cards in their own hands`` (CompletePlayerNames names) =
    startGame names |> fun game -> game.playerViews |> List.filter (fun view -> hasEmptyHand view.self) =! []

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``After starting the game, all players see cards in the hands of all other players`` (CompletePlayerNames names) =
    startGame names
    |> fun game -> game.playerViews
    |> List.filter (fun view -> view.otherPlayers |> List.exists hasEmptyHand) =! []

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Starting the game adds fuse and clock tokens to all views`` (CompletePlayerNames names) =
    let game = startGame names

    game.masterView.fuseTokens >! 0
    game.masterView.clockTokens >! 0
    game.playerViews |> List.filter (fun view -> view.fuseTokens = 0) =! []
    game.playerViews |> List.map (fun view -> view.clockTokens = 0) =! []
