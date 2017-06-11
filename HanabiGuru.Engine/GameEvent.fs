﻿namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of Player

module GameEvent =

    let processEvent state = function
        | PlayerJoined player -> GameState.addPlayer state player
    
    let toEventForPlayer player = function
        | PlayerJoined otherPlayer when otherPlayer <> player ->
            OtherPlayerJoined otherPlayer |> Some
        | PlayerJoined _ -> None
