module HanabiGuru.Engine.Tests.AddPlayersTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private select reason = function
    | CannotAddPlayer reasons -> List.filter ((=) reason) reasons
    | _ -> []

[<Property>]
let ``Players can be added until the game is full`` (players : Set<PlayerIdentity>) =
    let players = players |> Set.toList |> List.truncate GameRules.maximumPlayers
    players
    |> List.map Game.addPlayer
    |> List.fold GameAction.perform (Ok EventHistory.empty)
    |> Result.map GameState.players =! Ok (set players)

[<Property>]
let ``Players see themselves and all other players`` (players : Set<PlayerIdentity>) =
    let players = players |> Set.toList |> List.truncate GameRules.maximumPlayers
    players
    |> List.map Game.addPlayer
    |> List.fold GameAction.perform (Ok EventHistory.empty)
    |> Result.map (fun game -> List.map (fun player -> GameState.playerView player game) players)
    |> Result.map (List.map (fun view -> (PlayerView.self view) :: PlayerView.otherPlayers view |> List.sort))
        =! Ok (players |> List.sort |> List.replicate (List.length players))

[<Property>]
let ``All players agree on turn order`` (players : Set<PlayerIdentity>) =
    let players = players |> Set.toList |> List.truncate GameRules.maximumPlayers
    let gameOrError =
        players
        |> List.map Game.addPlayer
        |> List.fold GameAction.perform (Ok EventHistory.empty)

    let rotate distance (xs : PlayerIdentity list) = List.skip distance xs @ List.take distance xs 

    test <@ players
    |> List.map (fun player -> Result.map (GameState.playerView player) gameOrError)
    |> List.map (Result.map (fun view -> (PlayerView.self view) :: PlayerView.otherPlayers view))
    |> List.map (Result.map (fun turnOrder -> List.length players - 1 |> List.unfold (function
        | distance when distance >= 0 -> Some (rotate distance turnOrder, distance - 1)
        | _ -> None)))
    |> List.map (Result.map List.sort)
    |> List.map (Result.map List.head)
    |> Result.collect
    |> Result.map List.distinct
    |> Result.map List.length <= Ok 1 @>

[<Property>]
let ``Cannot add the same player more than once`` (player : PlayerIdentity) (PositiveInt repeats) =
    Game.addPlayer player
    |> List.replicate (1 + repeats)
    |> List.fold GameAction.perform (Ok EventHistory.empty)
    |> Result.mapError (select PlayerAlreadyJoined) =! Error [PlayerAlreadyJoined]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot add more than the maximum number of players`` (TooManyPlayers players) =
    players
    |> Set.toList
    |> List.map Game.addPlayer
    |> List.fold GameAction.perform (Ok EventHistory.empty) =! Error (CannotAddPlayer [NoSeatAvailable])

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot add a player after game has started`` (GameInProgress game) (player : PlayerIdentity) =
    Game.addPlayer player game
    |> Result.mapError (select CannotAddPlayerReason.GameAlreadyStarted)
        =! Error [CannotAddPlayerReason.GameAlreadyStarted]
