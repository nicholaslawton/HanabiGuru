﻿module HanabiGuru.Engine.GameState

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

let hands =
    EventHistory.choose (function
        | CardDealtToPlayer (card, player) -> Some (card, Some player, false)
        | CardDiscarded card -> Some (card, None, true)
        | _ -> None)
    >> List.groupBy (fun (card, _, _) -> card)
    >> List.map (fun (card, events) ->
        events
        |> List.fold
            (fun (_, player, discarded) (_, p, d) -> (card, player |> Option.orElse p, discarded || d))
            (card, None, false))
    >> List.choose (function
        | card, Some player, false -> Some (card, player)
        | _ -> None)
    >> List.groupBy snd
    >> List.map (Pair.mapSnd (List.map fst))
    >> List.map (fun (player, cards) -> PlayerHand.create player cards)

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
