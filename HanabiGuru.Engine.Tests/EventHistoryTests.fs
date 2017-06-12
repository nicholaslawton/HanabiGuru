module HanabiGuru.Engine.Tests.EventHistoryTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``Retrieving all history returns all events in the order in which they were recorded`` (events : GameEvent list) =
    let history = List.fold EventHistory.recordEvent EventHistory.empty events
    EventHistory.events history =! events
