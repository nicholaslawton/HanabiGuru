module HanabiGuru.Engine.Tests.GameStateTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The game always contains all cards`` (GameInProgress game) =
    let expectedCounts =
        [
            (Blue, Rank 1), 3
            (Blue, Rank 2), 2
            (Blue, Rank 3), 2
            (Blue, Rank 4), 2
            (Blue, Rank 5), 1
            (Green, Rank 1), 3
            (Green, Rank 2), 2
            (Green, Rank 3), 2
            (Green, Rank 4), 2
            (Green, Rank 5), 1
            (Red, Rank 1), 3
            (Red, Rank 2), 2
            (Red, Rank 3), 2
            (Red, Rank 4), 2
            (Red, Rank 5), 1
            (White, Rank 1), 3
            (White, Rank 2), 2
            (White, Rank 3), 2
            (White, Rank 4), 2
            (White, Rank 5), 1
            (Yellow, Rank 1), 3
            (Yellow, Rank 2), 2
            (Yellow, Rank 3), 2
            (Yellow, Rank 4), 2
            (Yellow, Rank 5), 1
        ]
        |> List.map (Pair.mapFst Card)
        |> List.sort

    let allCards game =
        let cardsInHands =
            GameState.hands game
            |> List.map (fun hand -> hand.cards |> List.map (fun { identity = card } -> card))
        GameState.drawDeck game :: cardsInHands
        |> List.collect id

    allCards game |> List.countBy id |> List.sort =! expectedCounts

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the fuse tokens`` (GameInProgress game) =
    let players = GameState.players game |> Set.toList
    List.map (fun player -> GameState.playerView player game) players
    |> List.map PlayerView.fuseTokens =! (List.map (fun _ -> GameState.fuseTokens game) players)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the clock tokens`` (GameInProgress game) =
    let players = GameState.players game |> Set.toList
    List.map (fun player -> GameState.playerView player game) players
    |> List.map PlayerView.clockTokens =! (List.map (fun _ -> GameState.clockTokens game) players)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the number of cards in the draw deck`` (GameInProgress game) =
    let players = GameState.players game |> Set.toList
    let expectedResult =
        GameState.drawDeck game
        |> List.length
        |> List.replicate (List.length players)

    List.map (fun player -> GameState.playerView player game) players
    |> List.map PlayerView.drawDeckSize =! expectedResult

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Each player always has five cards in a two or three player game`` (UpToThreePlayerGameInProgress game) =
    GameState.hands game
    |> List.map (fun hand -> (hand.player, List.length hand.cards))
        =! (GameState.players game |> Set.toList |> List.map (fun player -> (player, 5)))

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Each player always has four cards in a four or five player game`` (FourOrMorePlayerGameInProgress game) =
    GameState.hands game
    |> List.map (fun hand -> (hand.player, List.length hand.cards))
        =! (GameState.players game |> Set.toList |> List.map (fun player -> (player, 4)))

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the cards in the hands of the other players`` (GameInProgress game) =
    let players = GameState.players game |> Set.toList
    let expectedOtherHands player =
        GameState.hands game
        |> List.filter (fun hand -> hand.player <> player)
        |> List.sortBy (fun hand -> hand.player)

    players
    |> List.map (fun player ->
        let view = GameState.playerView player game
        view
        |> PlayerView.otherPlayers
        |> List.map (fun otherPlayer -> PlayerView.otherHand otherPlayer view)
        |> List.sortBy (fun hand -> hand.player))
    =! (List.map expectedOtherHands players)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``An unstarted game has no active player`` (GameReadyToStart game) =
    GameState.activePlayer game =! None

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``After starting the game, there is always an active player`` (GameInProgress game) =
    GameState.activePlayer game <>! None

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Play passes in turn order`` (GameInProgressAndNextTurn (game, nextTurn)) =
    let nextPlayer =
        GameState.activePlayer game
        |> Option.map (fun activePlayer ->
            GameState.playerView activePlayer game
            |> PlayerView.otherPlayers
            |> List.head)
    nextTurn game |> Result.map GameState.activePlayer =! Ok nextPlayer
