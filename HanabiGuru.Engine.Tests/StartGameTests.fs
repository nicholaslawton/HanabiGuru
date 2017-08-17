module HanabiGuru.Engine.Tests.GameTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private performAction historyOrError action = Result.bind action historyOrError
 
let private select reason = function
    | CannotStartGame reasons -> List.filter ((=) reason) reasons
    | _ -> []

[<Property>]
let ``Cannot start the game before the minimum number of players have joined`` (players : Set<PlayerIdentity>) =
    let addPlayers = players |> Set.toList |> List.truncate (GameRules.minimumPlayers - 1) |> List.map Game.addPlayer
    Game.startGame :: addPlayers
    |> List.rev
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.mapError (select WaitingForMinimumPlayers) =! Error [WaitingForMinimumPlayers]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game more than once returns an error`` (GameReadyToStart game) (PositiveInt repeats) =
    List.replicate (1 + repeats) Game.startGame
    |> List.fold performAction (Ok game)
    |> Result.mapError (select GameAlreadyStarted) =! Error [GameAlreadyStarted]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the fuse tokens to the game`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map GameState.fuseTokens =! Ok GameRules.fuseTokensAvailable

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the fuse tokens after starting the game`` (GameReadyToStart game) =
    let players = GameState.players game |> Set.toList
    Game.startGame game
    |> Result.map (fun game -> List.map (fun player -> GameState.playerView player game) players)
    |> Result.map (List.map PlayerView.fuseTokens) =! Ok (List.map (fun _ -> GameRules.fuseTokensAvailable) players)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the clock tokens to the game`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map GameState.clockTokens =! Ok GameRules.clockTokensAvailable

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the clock tokens after starting the game`` (GameReadyToStart game) =
    let players = GameState.players game |> Set.toList
    Game.startGame game
    |> Result.map (fun game -> List.map (fun player -> GameState.playerView player game) players)
    |> Result.map (List.map PlayerView.clockTokens) =! Ok (List.map (fun _ -> GameRules.clockTokensAvailable) players)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the standard set of cards to the game`` (GameReadyToStart game) =
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
        let cardsInHands = GameState.hands game |> List.map (fun hand -> hand.cards)
        GameState.drawDeck game :: cardsInHands
        |> List.collect id

    Game.startGame game
    |> Result.map (allCards >> List.countBy id >> List.sort) =! Ok expectedCounts

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals cards to each player`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map (GameState.hands >> List.map (fun hand -> hand.player)) =! Ok (GameState.players game |> Set.toList)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals five cards each for two or three players`` (UpToThreePlayerGameReadyToStart game) =
    Game.startGame game
    |> Result.map (GameState.hands >> List.map (fun hand -> hand.cards) >> List.map List.length)
        =! Ok (List.replicate (GameState.players game |> Set.count) 5)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals four cards each for four or five players`` (FourOrMorePlayerGameReadyToStart game) =
    Game.startGame game
    |> Result.map (GameState.hands >> List.map (fun hand -> hand.cards) >> List.map List.length)
        =! Ok (List.replicate (GameState.players game |> Set.count) 4)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals the initial hands non-deterministically`` (GameReadyToStart game) =
    List.replicate 5 game
    |> List.distinctBy Game.startGame
    |> List.length >! 1

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Players see the cards in the hands of the other players`` (GameReadyToStart game) =
    let players = GameState.players game |> Set.toList
    let startedGame = Game.startGame game
    let expectedOtherHands player =
        startedGame
        |> Result.map GameState.hands
        |> Result.map (List.filter (fun hand -> hand.player <> player))

    startedGame
    |> Result.map (fun game -> List.map (fun player -> GameState.playerView player game) players)
    |> Result.map (List.map PlayerView.otherHands)
        =! (List.map expectedOtherHands players |> Result.collect |> Result.mapError List.head)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game makes one of the players active`` (GameReadyToStart game) =
    Game.startGame game |> Result.map GameState.activePlayer <>! Ok None

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``An unstarted game has no active player`` (GameReadyToStart game) =
    game |> GameState.activePlayer =! None
