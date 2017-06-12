module HanabiGuru.Engine.Tests.PlayerViewTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``Sorting other players in relative turn order does not change the number of players`` (view : PlayerView) =
    let newView = PlayerView.sortOtherPlayersInRelativeTurnOrder view
    List.length newView.otherPlayers =! List.length view.otherPlayers

[<Property>]
let ``When other players are sorted in relative turn order, there is no more than one decrease in sequence``
    (view : PlayerView) =

    (PlayerView.sortOtherPlayersInRelativeTurnOrder view).otherPlayers
    |> List.pairwise
    |> List.filter (fun (x, y) -> x > y)
    |> List.length <! 2

[<Property>]
let ``Sorting other players in relative turn order places players following self at the front of the list in order``
    (view : PlayerView) =

    let newView = PlayerView.sortOtherPlayersInRelativeTurnOrder view
    let followingPlayers =
        newView.otherPlayers
        |> List.takeWhile ((<) view.self)
    let expectedFollowingPlayers =
        view.otherPlayers
        |> List.filter ((<) view.self)
        |> List.sort
    followingPlayers =! expectedFollowingPlayers

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``Sorting other players in relative turn order places players preceding self at the back of the list in order``
    (ValidPlayerView view) =

    let newView = PlayerView.sortOtherPlayersInRelativeTurnOrder view
    let precedingPlayers =
        newView.otherPlayers
        |> List.skipWhile ((<) view.self)
    let expectedPrecedingPlayers =
        view.otherPlayers
        |> List.filter ((>) view.self)
        |> List.sort
    precedingPlayers =! expectedPrecedingPlayers

[<Property>]
let ``After adding a player, the view contains one more player`` (view : PlayerView) (player : Player) =
    PlayerView.addOtherPlayer view player
    |> fun v -> v.otherPlayers
    |> List.length =! List.length view.otherPlayers + 1

[<Property>]
let ``After adding a player, the view contains the added player`` (view : PlayerView) (player : Player) =
    PlayerView.addOtherPlayer view player
    |> fun v -> v.otherPlayers
    |> List.filter ((=) player)
    |> List.length >! 0
