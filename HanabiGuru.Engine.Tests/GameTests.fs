module HanabiGuru.Engine.Tests.GameTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

[<Property>]
let ``When a player is added to the game, the event is recorded``
    (history : GameEvent list)
    (player : Player) =

    let recordEvent events event = event :: events
    let canAdd _ _ = []
    Game.addPlayer recordEvent canAdd history player =! (PlayerJoined player :: history, PlayerAdded)

[<Property>]
let ``When a player cannot be added to the game, the history is unchanged, and the reasons are returned``
    (history : GameEvent list)
    (player : Player)
    (reasonsArray : NonEmptyArray<CannotAddPlayerReason>) =

    let reasons = reasonsArray.Get |> Array.toList
    let recordEvent events event = event :: events
    let cannotAdd _ _ = reasons
    Game.addPlayer recordEvent cannotAdd history player =! (history, CannotAddPlayer reasons)

[<Property>]
let ``Can add a player who has not yet joined the game`` (history : EventHistory) (player : Player) =
    let notYetJoinedHistory = EventHistory.map (List.filter ((<>) (PlayerJoined player))) history
    Game.canAddPlayer player notYetJoinedHistory =! []

[<Property>]
let ``Cannot add a player who has already joined the game`` (history : EventHistory) (player : Player) =
    let alreadyJoinedHistory = EventHistory.recordEvent history (PlayerJoined player)
    Game.canAddPlayer player alreadyJoinedHistory =! [PlayerAlreadyJoined]
