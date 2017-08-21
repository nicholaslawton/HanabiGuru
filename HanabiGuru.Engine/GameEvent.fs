namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded
    | CardAddedToDrawDeck of Card
    | CardDealtToPlayer of Card * PlayerIdentity
    | StartTurn of PlayerIdentity

module GameEvent =
    
    let toEventForPlayer player = function
        | PlayerJoined self when player = self ->
            SelfJoined self |> Some
        | PlayerJoined otherPlayer ->
            OtherPlayerJoined otherPlayer |> Some
        | FuseTokenAdded ->
            PlayerEvent.FuseTokenAdded |> Some
        | ClockTokenAdded ->
            PlayerEvent.ClockTokenAdded |> Some
        | CardAddedToDrawDeck _ ->
            PlayerEvent.CardAddedToDrawDeck |> Some
        | CardDealtToPlayer (card, otherPlayer) when otherPlayer <> player ->
            CardDealtToOtherPlayer (card, otherPlayer) |> Some
        | CardDealtToPlayer _ ->
            CardDealtToSelf |> Some
        | StartTurn _ ->
            None
