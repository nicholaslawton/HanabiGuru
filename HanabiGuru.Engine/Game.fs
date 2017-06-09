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
        //let playerJoined = function
        //    | PlayerJoined _ -> true

        [
            PlayerAlreadyJoined, history |> EventHistory.apply (List.contains (PlayerJoined player))
            NoSeatAvailable, history
                //|> EventHistory.map (List.filter playerJoined)
                |> EventHistory.apply List.length >= playerLimit
        ]
        |> List.filter snd
        |> List.map fst
