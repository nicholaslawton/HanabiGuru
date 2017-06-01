module HanabiGuru.Engine.Tests.GameTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

open HanabiGuru.Engine
open HanabiGuru.Engine.Model

[<Property>]
let ``When a player is added to the game an event is added to the history``
    (history : GameEvent list)
    (player : Player) =

    let newHistory = Game.addPlayer history player
    newHistory =! PlayerJoined player :: history
