namespace HanabiGuru.Engine

type PlayerEvent =
    | OtherPlayerJoined of Player

module PlayerEvent =

    let apply view = function
        | OtherPlayerJoined player -> PlayerView.addOtherPlayer view player
