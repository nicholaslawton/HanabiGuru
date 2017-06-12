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

    let addPlayer canAdd history player =
        match canAdd player history with
        | [] -> PlayerJoined player |> Ok
        | reasons -> CannotAddPlayer reasons |> Error

    let canAddPlayer player history =
        //let playerJoined = function
        //    | PlayerJoined _ -> true

        [
            PlayerAlreadyJoined, history |> EventHistory.contains (PlayerJoined player)
            NoSeatAvailable, history
                //|> EventHistory.filter playerJoined
                |> EventHistory.length >= playerLimit
        ]
        |> List.filter snd
        |> List.map fst
