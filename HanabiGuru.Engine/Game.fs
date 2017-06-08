namespace HanabiGuru.Engine

type CannotAddPlayerReason =
    | PlayerAlreadyJoined

type AddPlayerResult =
    | PlayerAdded
    | CannotAddPlayer of CannotAddPlayerReason list

module Game =

    open HanabiGuru.Engine

    let addPlayer recordEvent canAdd history player =
        match canAdd player history with
        | [] -> PlayerJoined player |> recordEvent history, PlayerAdded
        | reasons -> history, CannotAddPlayer reasons

    let canAddPlayer player history =
        match EventHistory.apply (List.contains (PlayerJoined player)) history with
        | true -> [PlayerAlreadyJoined]
        | false -> []
