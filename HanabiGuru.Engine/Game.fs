namespace HanabiGuru.Engine

type CannotAddPlayerReason =
    | PlayerAlreadyJoined
    | NoSeatAvailable

type CannotPrepareDrawDeckReason =
    | DrawDeckAlreadyPrepared

type CannotPerformAction =
    | CannotAddPlayer of CannotAddPlayerReason list
    | CannotPrepareDrawDeck of CannotPrepareDrawDeckReason list

module Game =

    open HanabiGuru.Engine

    let playerLimit = 5

    let addPlayer canAdd history player =
        match canAdd player history with
        | [] -> PlayerJoined player |> List.singleton |> Ok
        | reasons -> reasons |> CannotAddPlayer |> Error

    let canAddPlayer player history =
        let isPlayerJoinedEvent = function
            | PlayerJoined _ -> true
            | _ -> false

        [
            PlayerAlreadyJoined, history |> EventHistory.contains (PlayerJoined player)
            NoSeatAvailable, history |> EventHistory.filter isPlayerJoinedEvent |> EventHistory.length >= playerLimit
        ]
        |> List.filter snd
        |> List.map fst

    let prepareDrawDeck history =
        let isCardAddedToDrawDeckEvent = function
            | CardAddedToDrawDeck _ -> true
            | _ -> false
        match EventHistory.exists isCardAddedToDrawDeckEvent history with
        | true -> [DrawDeckAlreadyPrepared] |> CannotPrepareDrawDeck |> Error
        | false ->
            let suits = [Blue; Green; Red; White; Yellow]
            let ranks = [1; 1; 1; 2; 2; 3; 3; 4; 4; 5] |> List.map Rank
            suits
            |> List.collect (fun suit -> ranks |> List.map (fun rank -> suit, rank))
            |> List.map Card
            |> List.map CardAddedToDrawDeck
            |> Ok
