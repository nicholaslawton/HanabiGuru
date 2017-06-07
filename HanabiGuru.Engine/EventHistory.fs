namespace HanabiGuru.Engine

type EventHistory = Events of GameEvent list

module EventHistory =

    let empty = Events []

    let apply f (Events events) = f events

    let map f events = apply f events |> Events

    let recordEvent events event = map (fun events -> event :: events) events

    let allEvents = apply List.rev
