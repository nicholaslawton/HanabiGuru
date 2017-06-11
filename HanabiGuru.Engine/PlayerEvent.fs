namespace HanabiGuru.Engine

type PlayerEvent =
    | OtherPlayerJoined of Player

module PlayerEvent =

    let processEvent state = function
        | OtherPlayerJoined player -> PlayerView.addOtherPlayer state player
