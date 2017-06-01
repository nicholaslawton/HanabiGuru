module HanabiGuru.Engine.Tests.GameEventTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

[<Property>]
let ``Processing a player joined event adds the new player to the game state`` (state : GameState) (player : Player) =
    let newState = GameEvent.processEvent state (PlayerJoined player)
    List.sort newState.players =! List.sort (player :: state.players)
