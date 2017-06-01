module HanabiGuru.Engine.Tests.GameTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

[<Property>]
let ``When a player is added to the game an event is added to the history``
    (history : EventHistory)
    (player : Player) =

    let initialEvents = EventHistory.allEvents history
    let newHistory = Game.addPlayer history player
    let newEvents = EventHistory.allEvents newHistory
    (PlayerJoined player :: initialEvents |> List.sort) =! List.sort newEvents
