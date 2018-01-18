module HanabiGuru.Engine.GameState

type State =
    | NotStarted
    | InProgress
    | Finished

let players = 
    EventHistory.choose (function 
        | PlayerJoined player -> Some player 
        | _ -> None) 

let fuseTokens _ = GameRules.totalFuseTokens

let clockTokens = EventHistory.sumBy (function
    | ClockTokenAdded -> 1
    | ClockTokenSpent -> -1
    | ClockTokenRestored -> 1
    | _ -> 0)

let card key = EventHistory.tryPick (function
    | CardDealtToPlayer ({ instanceKey = k; identity = card }, _) when k = key ->
        Some { instanceKey = key; identity = card}
    | _ -> None)

let drawDeck game =
    let cardsAddedToDrawDeck = 
        game
        |> EventHistory.choose (function
            | CardAddedToDrawDeck card -> Some card
            | _ -> None)
    let cardsDealt = 
        game
        |> EventHistory.choose (function
            | CardDealtToPlayer ({ identity = card }, _) -> Some card
            | _ -> None)
    List.removeEach cardsDealt cardsAddedToDrawDeck

let fireworks _ = []

let discard = EventHistory.choose (function
    | CardDiscarded { identity = card } -> Some card
    | _ -> None)

let hands game =
    let discardedCards = game |> EventHistory.choose (function
        | CardDiscarded card -> Some card
        | _ -> None)

    game
    |> EventHistory.choose (function
        | CardDealtToPlayer (card, player) when not <| List.contains card discardedCards -> Some (card, player)
        | _ -> None)
    |> List.groupBy snd
    |> List.map (Pair.mapSnd (List.map fst))
    |> List.map (fun (player, cards) -> PlayerHand.create player cards)

let activePlayer = EventHistory.tryPick (function
    | StartTurn player -> Some player
    | _ -> None)

let playerView player = EventHistory.choose (GameEvent.toEventForPlayer player)

let state game =
    let gameStarted =
        game
        |> EventHistory.exists (function
            | StartTurn _ -> true
            | _ -> false)
    let drawDeckExhausted =
        game
        |> EventHistory.sumBy (function
            | CardAddedToDrawDeck _ -> 1
            | CardDealtToPlayer _ -> -1
            | _ -> 0)
        |> (=) 0

    match (gameStarted, drawDeckExhausted) with
    | (false, _) -> NotStarted
    | (true, false) -> InProgress
    | (true, true) -> Finished
