module HanabiGuru.Engine.Tests.StartGameTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private select reason = function
    | CannotStartGame reasons -> List.filter ((=) reason) reasons
    | _ -> []

[<Property>]
let ``Cannot start the game before the minimum number of players have joined`` (players : Set<PlayerIdentity>) =
    let addPlayers = players |> Set.toList |> List.truncate (GameRules.minimumPlayers - 1) |> List.map Game.addPlayer
    Game.startGame :: addPlayers
    |> List.rev
    |> List.fold GameAction.perform (Ok EventHistory.empty)
    |> Result.mapError (select WaitingForMinimumPlayers) =! Error [WaitingForMinimumPlayers]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game more than once returns an error`` (GameReadyToStart game) (PositiveInt repeats) =
    List.replicate (1 + repeats) Game.startGame
    |> List.fold GameAction.perform (Ok game)
    |> Result.mapError (select GameAlreadyStarted) =! Error [GameAlreadyStarted]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the fuse tokens to the game`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map GameState.fuseTokens =! Ok GameRules.totalFuseTokens

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the clock tokens to the game`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map GameState.clockTokens =! Ok GameRules.totalClockTokens

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals the initial hands non-deterministically`` (GameReadyToStart game) =
    List.replicate 5 game
    |> List.distinctBy Game.startGame
    |> List.length >! 1
