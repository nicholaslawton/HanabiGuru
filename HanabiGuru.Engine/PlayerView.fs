module HanabiGuru.Engine.PlayerView

let self = List.pick (function
    | SelfJoined player -> Some player
    | _ -> None)

let otherPlayers view =
    view
    |> List.choose (function
        | SelfJoined player
        | OtherPlayerJoined player -> Some player
        | _ -> None)
    |> List.splitBy ((=) (self view))
    |> List.rev
    |> List.collect id

let drawDeckSize = List.sumBy (function
    | CardAddedToDrawDeck _ -> 1
    | CardDealtToSelf
    | CardDealtToOtherPlayer _ -> -1
    | _ -> 0)

let hand =
    List.choose (function
        | CardDealtToSelf -> Some ConcealedCard
        | _ -> None)

let otherHands =
    List.choose (function
        | CardDealtToOtherPlayer (card, otherPlayer) -> Some (card, otherPlayer)
        | _ -> None)
    >> List.groupBy snd
    >> List.map (Pair.mapSnd (List.map fst))
    >> List.map (fun (player, cards) -> PlayerHand.create player cards)

let fuseTokens _ = GameRules.fuseTokensAvailable

let clockTokens _ = GameRules.clockTokensAvailable

module CardIdentity =
    
    let deduce view _ =
        let unrevealedCards =
            view
            |> List.choose (function
                | CardAddedToDrawDeck card -> Some card
                | _ -> None)
            |> List.removeEach (List.choose (function
                | CardDealtToOtherPlayer (card, _) -> Some card
                | _ -> None) view)
            |> List.countBy id
        let unrevealedCount = List.sumBy snd unrevealedCards

        unrevealedCards
        |> List.map (fun (card, count) -> { card = card; probability = double count / double unrevealedCount })
        |> List.sortByDescending (fun candidate -> candidate.probability)
