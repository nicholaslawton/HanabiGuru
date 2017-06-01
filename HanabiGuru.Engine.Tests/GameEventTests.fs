module HanabiGuru.Engine.Tests.GameEventTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

open HanabiGuru.Engine
open HanabiGuru.Engine.Model

[<Property>]
let ``When a player joins the game, the new game state contains all players``
    (state : GameState)
    (player : Player) =

    let newState = GameEvent.processEvent state (PlayerJoined player)
    List.sort newState.players =! List.sort (player :: state.players)
