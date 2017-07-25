namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of PlayerIdentity
    | CardAddedToDrawDeck of Card
    | CardDealtToPlayer of Card * PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded

module GameEvent =

    let apply view = function
        | PlayerJoined player -> MasterView.addPlayer view player
        | CardAddedToDrawDeck card -> MasterView.addCardToDrawDeck view card
        | CardDealtToPlayer (card, player) -> MasterView.dealCardToPlayer view card player
        | FuseTokenAdded -> view
        | ClockTokenAdded -> view
    
    let toEventForPlayer player = function
        | PlayerJoined otherPlayer when otherPlayer <> player ->
            OtherPlayerJoined otherPlayer |> Some
        | PlayerJoined _ -> None
        | CardAddedToDrawDeck _ -> PlayerEvent.CardAddedToDrawDeck |> Some
        | CardDealtToPlayer (card, otherPlayer) when otherPlayer <> player ->
            CardDealtToOtherPlayer (card, otherPlayer) |> Some
        | CardDealtToPlayer _ -> CardDealtToSelf |> Some
        | FuseTokenAdded -> None
        | ClockTokenAdded -> None
