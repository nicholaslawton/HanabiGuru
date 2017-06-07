module HanabiGuru.Engine.Game

open HanabiGuru.Engine

let addPlayer recordEvent canAdd history player =
    match canAdd player history with
    | true -> PlayerJoined player |> recordEvent history
    | false -> history

let canAddPlayer player = EventHistory.apply (List.contains (PlayerJoined player)) >> not
