namespace HanabiGuru.Engine

type CannotAddPlayerReason =
    | PlayerAlreadyJoined
    | NoSeatAvailable

type AddPlayerResult =
    | PlayerAdded
    | CannotAddPlayer of CannotAddPlayerReason list

module Game =

    open HanabiGuru.Engine

    let playerLimit = 5

    let addPlayer recordEvent canAdd history player =
        match canAdd player history with
        | [] -> PlayerJoined player |> recordEvent history, PlayerAdded
        | reasons -> history, CannotAddPlayer reasons

    let canAddPlayer player history =
        let validate reasons = function
            | reason, false -> reason :: reasons
            | _, true -> reasons

        let filterPlayerJoinedEvents = List.filter (function
            | PlayerJoined _ -> true)

        [
            PlayerAlreadyJoined,
                history
                |> EventHistory.apply (List.contains (PlayerJoined player)) |> not
            NoSeatAvailable,
                history
                |> EventHistory.map filterPlayerJoinedEvents
                |> EventHistory.apply (List.length) < playerLimit
        ]
        |> List.fold validate []
