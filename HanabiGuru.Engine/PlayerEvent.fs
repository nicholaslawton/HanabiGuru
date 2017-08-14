namespace HanabiGuru.Engine

type PlayerEvent =
    | SelfJoined of PlayerIdentity
    | OtherPlayerJoined of PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded
    | CardAddedToDrawDeck
    | CardDealtToSelf
    | CardDealtToOtherPlayer of Card * PlayerIdentity

module PlayerEvent =

    let apply view _ = view
    (*
    let apply view = function
        | OtherPlayerJoined player -> PlayerView.addOtherPlayer view player
        | FuseTokenAdded -> PlayerView.addFuseToken view
        | ClockTokenAdded -> PlayerView.addClockToken view
        | CardAddedToDrawDeck -> PlayerView.addCardToDrawDeck view
        | CardDealtToSelf -> PlayerView.dealCardToSelf view
        | CardDealtToOtherPlayer (card, player) -> PlayerView.dealCardToOtherPlayer view card player
        *)
