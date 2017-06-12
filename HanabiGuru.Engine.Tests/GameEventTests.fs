module HanabiGuru.Engine.Tests.GameEventTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``Applying a player joined event adds the new player to the view`` (view : MasterView) (player : Player) =
    let newView = GameEvent.apply view (PlayerJoined player)
    List.sort newView.players =! List.sort (player :: view.players)
    
[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``A player joined event for another player is converted to a player event``
    (TwoPlayers (self, otherPlayer)) =

    let otherPlayerJoined = GameEvent.toEventForPlayer self (PlayerJoined otherPlayer)
    OtherPlayerJoined otherPlayer |> Some =! otherPlayerJoined

[<Property>]
let ``The self joined event is not converted to a player event`` (self : Player) =
    let selfJoined = GameEvent.toEventForPlayer self (PlayerJoined self)
    selfJoined =! None
