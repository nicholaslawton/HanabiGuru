namespace HanabiGuru.Engine

type PlayerEvent =
    | OtherPlayerJoined of Player
    | CardAddedToDrawDeck of Card

module PlayerEvent =

    let apply view = function
        | OtherPlayerJoined player -> PlayerView.addOtherPlayer view player
        | CardAddedToDrawDeck _ -> view
