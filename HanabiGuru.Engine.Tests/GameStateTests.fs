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
        GameState.drawDeck game
        :: GameState.fireworks game
        :: GameState.discard game
        :: cardsInHands
        |> List.collect id

    allCards game |> List.countBy id |> List.sort =! expectedCounts

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the fuse tokens`` (GameInProgress game) =
    let players = GameState.players game
    List.map (fun player -> GameState.playerView player game) players
    |> List.map PlayerView.fuseTokens =! (List.map (fun _ -> GameState.fuseTokens game) players)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``There is always at least one fuse token remaining in a game which is still in progress`` (GameInProgress game) =
    GameState.fuseTokens game >=! 1

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The number of fuse tokens remaining never increases`` (GameInProgressAndNextTurn (game, turn)) =
    GameState.fuseTokens game >=! (GameState.fuseTokens <| GameGeneration.executeTurn game turn)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the clock tokens`` (GameInProgress game) =
    let players = GameState.players game
    List.map (fun player -> GameState.playerView player game) players
    |> List.map PlayerView.clockTokens =! (List.map (fun _ -> GameState.clockTokens game) players)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The number of clock tokens remaining never drops below zero`` (GameInProgress game) =
    GameState.clockTokens game >=! 0

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The number of clock tokens remaining never exceeds the initial number available`` (GameInProgress game) =
    GameState.clockTokens game <=! GameRules.totalClockTokens

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the number of cards in the draw deck`` (GameInProgress game) =
    let players = GameState.players game
    let expectedResult =
        GameState.drawDeck game
        |> List.length
        |> List.replicate (List.length players)

    List.map (fun player -> GameState.playerView player game) players
    |> List.map PlayerView.drawDeckSize =! expectedResult

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``All players see all discarded cards sorted by identity`` (GameInProgress game) =
    let players = GameState.players game
    let expectedDiscard = GameState.discard game |> List.sort

    List.map (fun player -> GameState.playerView player game) players
    |> List.map PlayerView.discard =! (expectedDiscard |> List.replicate (List.length players))

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Each player always has five cards in a two or three player game while cards remain in the draw deck``
    (UpToThreePlayerGameInProgress game) =

    let handSizes =
        GameState.hands game
        |> List.map (fun hand -> (hand.player, List.length hand.cards))
        |> List.sort
    let validateHandSize size =
        if GameState.drawDeck game |> List.isEmpty
        then [4; 5]
        else [5]
        |> List.contains size
    test <@ handSizes |> List.forall (snd >> validateHandSize) @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Each player always has four cards in a four or five player game while cards remain in the draw deck``
    (FourOrMorePlayerGameInProgress game) =

    let handSizes =
        GameState.hands game
        |> List.map (fun hand -> (hand.player, List.length hand.cards))
        |> List.sort
    let validateHandSize size =
        if GameState.drawDeck game |> List.isEmpty
        then [3; 4]
        else [4]
        |> List.contains size
    test <@ handSizes |> List.forall (snd >> validateHandSize) @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players are holding the correct number of cards in their own hands`` (GameInProgress game) = 
    let players = GameState.players game
    players
    |> List.map (fun player -> player, GameState.playerView player game |> PlayerView.hand |> List.length)
    |> List.sort =! (GameState.hands game |> List.map (fun hand -> hand.player, List.length hand.cards) |> List.sort)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the cards in the hands of the other players`` (GameInProgress game) =
    let players = GameState.players game
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
    GameGeneration.executeTurn game nextTurn |> Result.map GameState.activePlayer =! Ok nextPlayer

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot take player turn before the game has started`` (players : Set<PlayerIdentity>) (PlayerTurn turn) =
    let players =
        players
        |> Set.toList
        |> List.truncate GameRules.maximumPlayers
    players
    |> List.map Game.addPlayer
    |> List.fold GameAction.perform (Ok EventHistory.empty)
    |> Result.bind (fun game -> GameGeneration.executeTurn game turn)
        =! Error (CannotTakeTurn [GameNotStarted])

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot take player turn once the game has ended`` (FinishedGame game) (PlayerTurn turn) =
    GameGeneration.executeTurn game turn =! Error (CannotTakeTurn [GameOver])

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Instance keys of cards dealt are unique`` (GameInProgress game) =
    game
    |> EventHistory.choose (function
        | CardDealtToPlayer (card, _) -> Some card.instanceKey
        | _ -> None)
    |> List.countBy id
    |> List.filter (snd >> ((<>) 1)) =! []
