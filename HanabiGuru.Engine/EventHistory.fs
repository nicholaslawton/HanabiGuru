namespace HanabiGuru.Engine

type EventHistory = Events of GameEvent list

module EventHistory =

    let empty = Events []

    let recordEvent (Events events) event = event :: events |> Events

    let allEvents (Events events) = List.rev events
