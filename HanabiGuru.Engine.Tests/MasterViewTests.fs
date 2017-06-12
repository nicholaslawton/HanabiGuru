module HanabiGuru.Engine.Tests.MasterViewTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``After adding a player, all players are in turn order`` (view : MasterView) (player : Player) =
    let newView = MasterView.addPlayer view player
    player :: view.players |> List.sort =! newView.players
