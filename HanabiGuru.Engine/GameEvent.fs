namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of Player

module GameEvent =

    let apply view = function
        | PlayerJoined player -> MasterView.addPlayer view player
    
    let toEventForPlayer player = function
        | PlayerJoined otherPlayer when otherPlayer <> player ->
            OtherPlayerJoined otherPlayer |> Some
        | PlayerJoined _ -> None
