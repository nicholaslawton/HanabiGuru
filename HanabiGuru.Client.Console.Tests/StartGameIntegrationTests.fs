module HanabiGuru.Client.Console.Tests.StartGameIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private hasEmptyHand player = List.isEmpty player.cards

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Starting a game deals cards to all players`` (ValidNames names) =
    CommandProcessingTestFramework.startGame names |> GameState.hands |> List.filter hasEmptyHand =! []

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``After starting a game, each player has a hand of concealed cards`` (ValidNames names) =
    let startedGame = CommandProcessingTestFramework.startGame names
    GameState.players startedGame
    |> List.map (fun player -> GameState.playerView player startedGame)
    |> List.map PlayerView.hand
    |> List.forall (not << List.isEmpty)

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``After starting a game, all players see cards in the hands of all other players`` (ValidNames names) =
    let startedGame = CommandProcessingTestFramework.startGame names
    GameState.players startedGame
    |> List.map (fun player -> GameState.playerView player startedGame)
    |> List.map (fun view ->
        PlayerView.otherPlayers view
        |> List.map (fun otherPlayer -> PlayerView.otherHand otherPlayer view))
    |> List.collect id
    |> List.filter hasEmptyHand =! []

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Starting a game adds fuse tokens`` (ValidNames names) =
    CommandProcessingTestFramework.startGame names |> GameState.fuseTokens >! 0

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Starting a game adds clock tokens`` (ValidNames names) =
    CommandProcessingTestFramework.startGame names |> GameState.clockTokens >! 0
