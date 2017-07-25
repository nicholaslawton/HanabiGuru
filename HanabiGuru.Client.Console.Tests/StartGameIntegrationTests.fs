﻿module HanabiGuru.Client.Console.Tests.StartGameIntegrationTests

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
    (startGame names).masterView.players |> List.filter hasEmptyHand =! []

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``After starting the game, all players have cards in their own hands`` (CompletePlayerNames names) =
    (startGame names).playerViews |> List.filter (fun view -> hasEmptyHand view.self) =! []

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``After starting the game, all players see cards in the hands of all other players`` (CompletePlayerNames names) =
    (startGame names).playerViews |> List.filter (fun view -> view.otherPlayers |> List.exists hasEmptyHand) =! []

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Starting the game adds fuse tokens to master view`` (CompletePlayerNames names) =
    (startGame names).masterView.fuseTokens >! 0

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Starting the game adds clock tokens to master view`` (CompletePlayerNames names) =
    (startGame names).masterView.clockTokens >! 0

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Starting the game adds fuse tokens to all player views`` (CompletePlayerNames names) =
    (startGame names).playerViews |> List.filter (fun view -> view.fuseTokens = 0) =! []

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Starting the game adds clock tokens to all player views`` (CompletePlayerNames names) =
    (startGame names).playerViews |> List.filter (fun view -> view.clockTokens = 0) =! []
