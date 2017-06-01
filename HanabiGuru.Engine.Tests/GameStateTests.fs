module HanabiGuru.Engine.Tests.GameStateTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

[<Property>]
let ``After adding a player, all players are in turn order`` (state : GameState) (player : Player) =
    let newState = GameState.addPlayer state player
    player :: state.players |> List.sort =! newState.players
