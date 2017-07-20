﻿namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of Player
    | CardAddedToDrawDeck of Card
    | CardDealtToPlayer of Card * PlayerIdentity

module GameEvent =

    let apply view = function
        | PlayerJoined player -> MasterView.addPlayer view player
        | CardAddedToDrawDeck card -> MasterView.addCardToDrawDeck view card
        | CardDealtToPlayer _ -> view
    
    let toEventForPlayer player = function
        | PlayerJoined otherPlayer when otherPlayer <> player ->
            OtherPlayerJoined otherPlayer |> Some
        | PlayerJoined _ -> None
        | CardAddedToDrawDeck _ -> PlayerEvent.CardAddedToDrawDeck |> Some
        | CardDealtToPlayer _ -> None
