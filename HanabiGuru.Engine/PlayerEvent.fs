namespace HanabiGuru.Engine

type PlayerEvent =
    | OtherPlayerJoined of Player

module PlayerEvent =

    let processEvent view = function
        | OtherPlayerJoined player -> PlayerView.addOtherPlayer view player
