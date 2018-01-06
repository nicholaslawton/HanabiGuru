module HanabiGuru.Engine.GameState

let players = 
    EventHistory.choose (function 
        | PlayerJoined player -> Some player 
        | _ -> None) 

let fuseTokens _ = GameRules.fuseTokensAvailable

let clockTokens = EventHistory.sumBy (function
    | ClockTokenAdded -> 1
    | ClockTokenSpent -> -1
    | _ -> 0)

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

let hands game =
    game
    |> EventHistory.choose (function
        | CardDealtToPlayer (card, player) -> Some (card, player)
        | _ -> None)
    |> List.groupBy snd
    |> List.map (Pair.mapSnd (List.map fst))
    |> List.map (fun (player, cards) -> PlayerHand.create player cards)

let fireworks _ = []

let discard _ = []

let activePlayer = EventHistory.tryPick (function
    | StartTurn player -> Some player
    | _ -> None)

let playerView player = EventHistory.choose (GameEvent.toEventForPlayer player)
