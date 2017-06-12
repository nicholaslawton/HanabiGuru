namespace HanabiGuru.Engine

type CannotAddPlayerReason =
    | PlayerAlreadyJoined
    | NoSeatAvailable

module Game =

    open HanabiGuru.Engine

    let playerLimit = 5

    let addPlayer canAdd history player =
        match canAdd player history with
        | [] -> PlayerJoined player |> Ok
        | reasons -> Error reasons

    let canAddPlayer player history =
        [
            PlayerAlreadyJoined, history |> EventHistory.contains (PlayerJoined player)
            NoSeatAvailable, history |> EventHistory.length >= playerLimit
        ]
        |> List.filter snd
        |> List.map fst
