namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded
    | CardAddedToDrawDeck of Card
    | CardDealtToPlayer of Card * PlayerIdentity
    | StartTurn of PlayerIdentity

module GameEvent =

    let apply view = function
        | PlayerJoined player -> MasterView.addPlayer view player
        | FuseTokenAdded -> MasterView.addFuseToken view
        | ClockTokenAdded -> MasterView.addClockToken view
        | CardAddedToDrawDeck card -> MasterView.addCardToDrawDeck view card
        | CardDealtToPlayer (card, player) -> MasterView.dealCardToPlayer view card player
        | StartTurn player -> MasterView.startTurn view player
    
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
