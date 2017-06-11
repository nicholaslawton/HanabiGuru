﻿module HanabiGuru.Engine.Tests.GameEventTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``Processing a player joined event adds the new player to the game state`` (state : GameState) (player : Player) =
    let newState = GameEvent.processEvent state (PlayerJoined player)
    List.sort newState.players =! List.sort (player :: state.players)
    
[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``A player joined event for another player is converted to a player event``
    (TwoPlayers (self, otherPlayer)) =

    let otherPlayerJoined = GameEvent.toEventForPlayer self (PlayerJoined otherPlayer)
    OtherPlayerJoined otherPlayer |> Some =! otherPlayerJoined

[<Property>]
let ``The self joined event is not converted to a player event`` (self : Player) =
    let selfJoined = GameEvent.toEventForPlayer self (PlayerJoined self)
    selfJoined =! None
