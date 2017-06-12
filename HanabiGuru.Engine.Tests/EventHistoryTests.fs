module HanabiGuru.Engine.Tests.EventHistoryTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``Recording and retrieving events returns them in the order they were recorded`` (events : GameEvent list) =
    events |> List.fold EventHistory.recordEvent EventHistory.empty |> EventHistory.events =! events
