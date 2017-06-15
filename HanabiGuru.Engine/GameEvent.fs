namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of Player
    | CardAddedToDrawDeck of Card

module GameEvent =

    let apply view = function
        | PlayerJoined player -> MasterView.addPlayer view player
        | CardAddedToDrawDeck card -> MasterView.addCardToDrawDeck view card
    
    let toEventForPlayer player = function
        | PlayerJoined otherPlayer when otherPlayer <> player ->
            OtherPlayerJoined otherPlayer |> Some
        | PlayerJoined _ -> None
        | CardAddedToDrawDeck _ -> PlayerEvent.CardAddedToDrawDeck |> Some
