namespace HanabiGuru.Engine

type PlayerEvent =
    | OtherPlayerJoined of Player
    | CardAddedToDrawDeck

module PlayerEvent =

    let apply view = function
        | OtherPlayerJoined player -> PlayerView.addOtherPlayer view player
        | CardAddedToDrawDeck -> PlayerView.addCardToDrawDeck view
