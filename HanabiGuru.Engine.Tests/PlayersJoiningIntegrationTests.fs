module HanabiGuru.Engine.Tests.PlayersJoiningIntegrationTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

[<Property>]
let ``All players which join the game are added to the state in turn order`` (players : Player list) =
    let history = List.fold Game.addPlayer EventHistory.empty players
    let events = EventHistory.allEvents history
    let state = List.fold GameEvent.processEvent GameState.initial events
    state.players =! List.sort players
