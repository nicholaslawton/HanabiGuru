module HanabiGuru.Engine.Tests.GameTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``When a player is added to the game, the event is returned``
    (history : GameEvent list)
    (player : Player) =

    let canAdd _ _ = []
    Game.addPlayer canAdd history player =! (PlayerJoined player |> Ok)

[<Property>]
let ``When a player cannot be added to the game the reasons are returned``
    (history : GameEvent list)
    (player : Player)
    (reasonsArray : NonEmptyArray<CannotAddPlayerReason>) =

    let reasons = reasonsArray.Get |> Array.toList
    let cannotAdd _ _ = reasons
    Game.addPlayer cannotAdd history player =! (Error reasons)

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Can add a player who has not yet joined the game when there is a seat available``
    (CanAddPlayerArrangement (newPlayer, seatedPlayers)) =

    seatedPlayers
    |> List.map PlayerJoined
    |> List.fold EventHistory.recordEvent EventHistory.empty
    |> Game.canAddPlayer newPlayer =! []

[<Property>]
let ``Cannot add a player who has already joined the game`` (history : EventHistory) (player : Player) =
    let alreadyJoinedHistory = EventHistory.recordEvent history (PlayerJoined player)
    let reasons = Game.canAddPlayer player alreadyJoinedHistory
    reasons |> List.contains PlayerAlreadyJoined
        |@ sprintf "%A does not contain %A" reasons PlayerAlreadyJoined

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Cannot add a player when there is no seat available``
    (TooManyPlayers (newPlayer, seatedPlayers)) =

    let history = seatedPlayers |> List.map PlayerJoined |> List.fold EventHistory.recordEvent EventHistory.empty
    let reasons = Game.canAddPlayer newPlayer history
    reasons |> List.contains NoSeatAvailable
        |@ sprintf "%A does not contain %A" reasons NoSeatAvailable
