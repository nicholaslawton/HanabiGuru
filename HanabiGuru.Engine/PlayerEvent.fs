namespace HanabiGuru.Engine

open Model

type PlayerEvent =
    | OtherPlayerJoined of Player

module PlayerEvent =

    let processEvent state = function
        | PlayerJoined player -> PlayerView.addOtherPlayer state player
