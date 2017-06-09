namespace HanabiGuru.Engine

type EventHistory = Events of GameEvent list

module EventHistory =

    let empty = Events []

    let apply f (Events events) = f events

    let map f events = apply f events |> Events

    let recordEvent history event = map (fun events -> event :: events) history

    let allEvents = apply List.rev
