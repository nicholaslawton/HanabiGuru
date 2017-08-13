module HanabiGuru.Engine.Tests.GameTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private performAction historyOrError action = Result.bind action historyOrError

let private mapCannotPerformActionReasons fCannotStartGame = function
    | CannotStartGame reasons -> fCannotStartGame reasons
    | _ -> []
 
let private selectReason reason = List.filter ((=) reason) 
 
let private selectCannotStartGameReason reason = 
    mapCannotPerformActionReasons (selectReason reason)

[<Property>]
let ``Cannot start the game before the minimum number of players have joined`` (players : Set<PlayerIdentity>) =
    let addPlayers = players |> Set.toList |> List.truncate (GameRules.minimumPlayers - 1) |> List.map Game.addPlayer
    Game.startGame :: addPlayers
    |> List.rev
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.mapError (selectCannotStartGameReason CannotStartGameReason.WaitingForMinimumPlayers)
        =! Error [CannotStartGameReason.WaitingForMinimumPlayers]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game more than once returns an error`` (GameReadyToStart game) (PositiveInt repeats) =
    List.replicate (1 + repeats) Game.startGame
    |> List.fold performAction (Ok game)
    |> Result.mapError (selectCannotStartGameReason CannotStartGameReason.GameAlreadyStarted)
        =! Error [CannotStartGameReason.GameAlreadyStarted]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the fuse tokens to the game`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map GameState.fuseTokens =! Ok GameRules.fuseTokensAvailable

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the clock tokens to the game`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map GameState.clockTokens =! Ok GameRules.clockTokensAvailable

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
        let cardsInHands = GameState.hands game |> List.map (fun hand -> hand.hand)
        GameState.drawDeck game :: cardsInHands
        |> List.collect id

    Game.startGame game
    |> Result.map (allCards >> List.countBy id >> List.sort) =! Ok expectedCounts

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals five cards each for two or three players`` (UpToThreePlayerGameReadyToStart game) =
    Game.startGame game
    |> Result.map (GameState.hands >> List.map (fun hand -> hand.hand) >> List.map List.length)
        =! Ok (List.replicate (GameState.players game |> Set.count) 5)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals four cards each for four or five players`` (FourOrMorePlayerGameReadyToStart game) =
    Game.startGame game
    |> Result.map (GameState.hands >> List.map (fun hand -> hand.hand) >> List.map List.length)
        =! Ok (List.replicate (GameState.players game |> Set.count) 4)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals the initial hands non-deterministically`` (GameReadyToStart game) =
    List.replicate 5 game
    |> List.distinctBy Game.startGame
    |> List.length >! 1

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game starts the first turn`` (GameReadyToStart game) =
    let firstPlayer = game |> GameState.players |> Set.toList |> List.sort |> List.tryHead
    Game.startGame game
    |> Result.map GameState.activePlayer =! Ok firstPlayer
