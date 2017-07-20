﻿module HanabiGuru.Client.Console.Tests.StartGameIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Client.Console

[<Property>]
let ``After starting the game, all players have cards in their hands`` (names : string list) =
    let game = 
        names
        |> List.map AddPlayer
        |> List.append [StartGame]
        |> CommandExecutionTestFramework.execute

    let hasEmptyHand player = List.isEmpty player.hand

    [
        game.masterView.players |> List.filter hasEmptyHand =! []
            |@ "No empty hands in master view"
        game.playerViews |> List.filter (fun view -> hasEmptyHand view.self) =! []
            |@ "No empty hands for self"
        game.playerViews |> List.filter (fun view -> view.otherPlayers |> List.exists hasEmptyHand) =! []
            |@ "No empty hands for other players"
    ]