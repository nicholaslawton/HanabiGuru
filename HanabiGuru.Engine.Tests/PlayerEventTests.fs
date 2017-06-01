module HanabiGuru.Engine.Tests.PlayerEventTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Engine.Model

[<Property>]
let ``Processing a player joined event returns the new player view with all other players``
    (view : PlayerView)
    (player : Player) =

    let newView = PlayerEvent.processEvent view (PlayerJoined player)
    List.sort newView.otherPlayers =! List.sort (player :: view.otherPlayers)
