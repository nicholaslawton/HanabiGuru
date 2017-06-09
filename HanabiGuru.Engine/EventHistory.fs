namespace HanabiGuru.Engine

type EventHistory = Events of GameEvent list

module EventHistory =

    let empty = Events []

    let private apply f (Events events) = f events

    let map f events = apply f events |> Events

    let recordEvent history event = map (fun events -> event :: events) history

    let allEvents = apply List.rev

    let length = apply List.length

    let contains event = apply (List.contains event)
