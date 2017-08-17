module HanabiGuru.Client.Console.Tests.StartGameIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private startGame names =
    let addPlayers = names |> Set.toList |> List.map (sprintf "add player %s")
    "start" :: addPlayers |> List.rev |> CommandProcessingTestFramework.processInput

let private hasEmptyHand player = List.isEmpty player.cards

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Starting a game deals cards to all players`` (Names names) =
    startGame names |> GameState.hands |> List.filter hasEmptyHand =! []

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``After starting a game, all players see cards in the hands of all other players`` (Names names) =
    let startedGame = startGame names
    let players = GameState.players startedGame
    let playerViews = players |> Set.toList |> List.map (fun player -> GameState.playerView player startedGame)
    let otherHands = List.map PlayerView.otherHands playerViews
    otherHands |> List.collect id |> List.filter hasEmptyHand =! []

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Starting a game adds fuse tokens`` (Names names) =
    startGame names |> GameState.fuseTokens >! 0

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Starting a game adds clock tokens`` (Names names) =
    startGame names |> GameState.clockTokens >! 0
