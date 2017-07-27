﻿namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded
    | CardAddedToDrawDeck of Card
    | CardDealtToPlayer of Card * PlayerIdentity
    | NextTurn of PlayerIdentity

module GameEvent =

    let apply view = function
        | PlayerJoined player -> MasterView.addPlayer view player
        | FuseTokenAdded -> MasterView.addFuseToken view
        | ClockTokenAdded -> MasterView.addClockToken view
        | CardAddedToDrawDeck card -> MasterView.addCardToDrawDeck view card
        | CardDealtToPlayer (card, player) -> MasterView.dealCardToPlayer view card player
        | NextTurn _ -> view
    
    let toEventForPlayer player = function
        | PlayerJoined otherPlayer when otherPlayer <> player ->
            OtherPlayerJoined otherPlayer |> Some
        | PlayerJoined _ -> None
        | FuseTokenAdded -> PlayerEvent.FuseTokenAdded |> Some
        | ClockTokenAdded -> PlayerEvent.ClockTokenAdded |> Some
        | CardAddedToDrawDeck _ -> PlayerEvent.CardAddedToDrawDeck |> Some
        | CardDealtToPlayer (card, otherPlayer) when otherPlayer <> player ->
            CardDealtToOtherPlayer (card, otherPlayer) |> Some
        | CardDealtToPlayer _ -> CardDealtToSelf |> Some
        | NextTurn _ -> None
