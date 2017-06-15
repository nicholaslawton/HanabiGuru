﻿module HanabiGuru.Engine.Tests.GameEventTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
    
[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``A player joined event for another player is converted to a player event`` (TwoPlayers (self, otherPlayer)) =
    GameEvent.toEventForPlayer self (PlayerJoined otherPlayer) =! (OtherPlayerJoined otherPlayer |> Some)

[<Property>]
let ``The self joined event is not converted to a player event`` (self : Player) =
    GameEvent.toEventForPlayer self (PlayerJoined self) =! None

[<Property>]
let ``A card added to draw deck event is converted to a player event`` (player : Player) (card : Card) =
    GameEvent.toEventForPlayer player (GameEvent.CardAddedToDrawDeck card) =! Some PlayerEvent.CardAddedToDrawDeck
