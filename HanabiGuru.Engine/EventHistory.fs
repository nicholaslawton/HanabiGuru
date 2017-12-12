namespace HanabiGuru.Engine

type EventHistory = Events of GameEvent list

module EventHistory =

    let empty = Events []

    let private apply f (Events events) = f events

    let map f events = apply f events |> Events

    let recordEvent history event = map (fun events -> event :: events) history

    let recordEvents = List.fold recordEvent

    let events = apply List.rev

    let length = apply List.length

    let contains = List.contains >> apply

    let filter = List.filter >> map

    let countOf f history = history |> filter f |> length

    let exists = List.exists >> apply

    let choose f = events >> List.choose f

    let tryPick f = List.tryPick f |> apply

    let inline sumBy f = events >> List.sumBy f
