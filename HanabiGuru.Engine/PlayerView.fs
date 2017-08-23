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
    | CardAddedToDrawDeck -> 1
    | CardDealtToSelf
    | CardDealtToOtherPlayer _ -> -1
    | _ -> 0)

let hand _ = [ConcealedCard [true]]

let otherHands view =
    view
    |> List.choose (function
        | CardDealtToOtherPlayer (card, otherPlayer) -> Some (card, otherPlayer)
        | _ -> None)
    |> List.groupBy snd
    |> List.map (Pair.mapSnd (List.map fst))
    |> List.map (fun (player, cards) -> PlayerHand.create player cards)

let fuseTokens _ = GameRules.fuseTokensAvailable

let clockTokens _ = GameRules.clockTokensAvailable
