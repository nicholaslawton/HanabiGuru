namespace HanabiGuru.Engine

type PlayerEvent =
    | OtherPlayerJoined of PlayerIdentity
    | CardAddedToDrawDeck
    | CardDealtToSelf
    | CardDealtToOtherPlayer of Card * PlayerIdentity

module PlayerEvent =

    let apply view = function
        | OtherPlayerJoined player -> PlayerView.addOtherPlayer view player
        | CardAddedToDrawDeck -> PlayerView.addCardToDrawDeck view
        | CardDealtToSelf -> PlayerView.dealCardToSelf view
        | CardDealtToOtherPlayer (card, player) -> PlayerView.dealCardToOtherPlayer view card player
