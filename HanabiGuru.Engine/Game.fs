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

    let private canPerformAction history rules =
        rules
        |> List.filter (snd >> fun rule -> rule history)
        |> List.map fst

    let private performAction rules action createReasons history =
        match canPerformAction history rules with
        | [] -> action () |> Ok
        | reasons -> reasons |> createReasons |> Error

    let addPlayer player =
        let isPlayerJoined = function
            | PlayerJoined _ -> true
            | _ -> false
        let rules =
            [
                PlayerAlreadyJoined, EventHistory.contains (PlayerJoined player)
                NoSeatAvailable, EventHistory.filter isPlayerJoined >> EventHistory.length >> ((<=) playerLimit)
            ]

        let createEvents () = PlayerJoined player |> List.singleton

        performAction rules createEvents CannotAddPlayer

    let prepareDrawDeck history =
        let isCardAddedToDrawDeck = function
            | CardAddedToDrawDeck _ -> true
            | _ -> false
        let rules = [ DrawDeckAlreadyPrepared, EventHistory.exists isCardAddedToDrawDeck ]

        let createEvents () =
            let suits = [Blue; Green; Red; White; Yellow]
            let ranks = [1; 1; 1; 2; 2; 3; 3; 4; 4; 5] |> List.map Rank
            suits
            |> List.collect (fun suit -> ranks |> List.map (fun rank -> suit, rank))
            |> List.map Card
            |> List.map CardAddedToDrawDeck

        performAction rules createEvents CannotPrepareDrawDeck history
