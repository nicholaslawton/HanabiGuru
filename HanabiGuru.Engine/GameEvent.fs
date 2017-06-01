namespace HanabiGuru.Engine

open Model

type GameEvent =
    | PlayerJoined of Player

module GameEvent =

    let processEvent state = function
        | PlayerJoined player -> GameState.playerJoined state player
