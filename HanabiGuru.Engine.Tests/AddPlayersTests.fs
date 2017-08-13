module HanabiGuru.Engine.Tests.AddPlayersTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private performAction historyOrError action = Result.bind action historyOrError

let private mapCannotPerformActionReasons fCannotAddPlayer = function
    | CannotAddPlayer reasons -> fCannotAddPlayer reasons 
    | _ -> []
 
let private selectReason reason = List.filter ((=) reason) 

let private selectCannotAddPlayerReason reason = 
    mapCannotPerformActionReasons (selectReason reason)

[<Property>]
let ``Players can be added until the game is full`` (players : Set<PlayerIdentity>) =
    let players = players |> Set.toList |> List.truncate GameRules.maximumPlayers
    players
    |> List.map Game.addPlayer
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.map GameState.players =! Ok (set players)

[<Property>]
let ``Cannot add the same player more than once`` (player : PlayerIdentity) (PositiveInt repeats) =
    Game.addPlayer player
    |> List.replicate (1 + repeats)
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.mapError (selectCannotAddPlayerReason PlayerAlreadyJoined) =! Error [PlayerAlreadyJoined]

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Cannot add more than the maximum number of players`` (TooManyPlayers (newPlayer, seatedPlayers)) =
    newPlayer :: seatedPlayers
    |> List.rev
    |> List.map Game.addPlayer
    |> List.fold performAction (Ok EventHistory.empty) =! Error (CannotAddPlayer [NoSeatAvailable])

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot add a player after game has started`` (GameReadyToStart game) (player : PlayerIdentity) =
    Game.startGame :: [Game.addPlayer player]
    |> List.fold performAction (Ok game)
    |> Result.mapError (selectCannotAddPlayerReason CannotAddPlayerReason.GameAlreadyStarted)
        =! Error [CannotAddPlayerReason.GameAlreadyStarted]
