module HanabiGuru.Engine.Tests.PlayerEventTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``Applying a player joined event returns the new player view with all other players``
    (view : PlayerView)
    (player : Player) =

    let newView = PlayerEvent.apply view (OtherPlayerJoined player)
    List.sort newView.otherPlayers =! List.sort (player :: view.otherPlayers)
