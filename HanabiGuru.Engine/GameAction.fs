module HanabiGuru.Engine.GameAction

let dealInitialHands drawDeck players =
    let playerCount = Set.count players
    let handSize = if playerCount <= 3 then 5 else 4
    players
    |> Set.toList
    |> List.replicate handSize
    |> List.collect id
    |> List.zip (drawDeck |> List.sortBy (ignore >> Random.double) |> List.take (playerCount * handSize))

    (*
let advanceTurn history =
    let getPlayerJoined = function
        | PlayerJoined player -> Some player
        | _ -> None

    let rules = [ GameNotStarted, not << EventHistory.exists isCardDealtToPlayer ]

    let createEvents () =
        Seq.initInfinite (fun _ -> EventHistory.choose getPlayerJoined history |> List.sort)
        |> Seq.collect id
        |> Seq.skip (EventHistory.countOf isNextTurn history)
        |> Seq.take 1
        |> Seq.map StartTurn
        |> List.ofSeq

    performAction rules createEvents CannotAdvanceTurn history
    *)
