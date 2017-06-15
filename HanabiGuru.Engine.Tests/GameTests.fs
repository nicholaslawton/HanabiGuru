module HanabiGuru.Engine.Tests.GameTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``When a player is added to the game, the event is returned`` (history : GameEvent list) (player : Player) =
    let canAdd _ _ = []
    Game.addPlayer canAdd history player =! Ok [PlayerJoined player]

[<Property>]
let ``When a player cannot be added to the game, the reasons are returned``
    (history : GameEvent list)
    (player : Player)
    (reasonsArray : NonEmptyArray<CannotAddPlayerReason>) =

    let reasons = reasonsArray.Get |> Array.toList
    let cannotAdd _ _ = reasons
    Game.addPlayer cannotAdd history player =! Error reasons

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Can add a player who has not yet joined the game when there is a seat available``
    (events : GameEvent list)
    (CanAddPlayerArrangement (newPlayer, seatedPlayers)) =

    let history =
        events
        |> List.filter (function
            | PlayerJoined _ -> false
            | _ -> true)
        |> List.fold EventHistory.recordEvent EventHistory.empty

    seatedPlayers
    |> List.map PlayerJoined
    |> List.fold EventHistory.recordEvent history
    |> Game.canAddPlayer newPlayer =! []

[<Property>]
let ``Cannot add a player after they have already joined the game`` (history : EventHistory) (player : Player) =
    EventHistory.recordEvent history (PlayerJoined player)
    |> Game.canAddPlayer player
    |> List.filter ((=) PlayerAlreadyJoined) = [PlayerAlreadyJoined]

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Cannot add a player when there is no seat available`` (TooManyPlayers (newPlayer, seatedPlayers)) =
    seatedPlayers
    |> List.map PlayerJoined
    |> List.fold EventHistory.recordEvent EventHistory.empty
    |> Game.canAddPlayer newPlayer
    |> List.filter ((=) NoSeatAvailable) =! [NoSeatAvailable]

[<Fact>]
let ``Preparing the draw deck creates the events`` () =
    let countBySuitAndRank = List.countBy (function
        | (GameEvent.CardAddedToDrawDeck (Card (suit, rank))) -> suit, rank
        | _ -> new AssertionFailedException("Unexpected event") |> raise)
    let expectedCounts =
        [
            (Blue, Rank 1), 3
            (Blue, Rank 2), 2
            (Blue, Rank 3), 2
            (Blue, Rank 4), 2
            (Blue, Rank 5), 1
            (Green, Rank 1), 3
            (Green, Rank 2), 2
            (Green, Rank 3), 2
            (Green, Rank 4), 2
            (Green, Rank 5), 1
            (Red, Rank 1), 3
            (Red, Rank 2), 2
            (Red, Rank 3), 2
            (Red, Rank 4), 2
            (Red, Rank 5), 1
            (White, Rank 1), 3
            (White, Rank 2), 2
            (White, Rank 3), 2
            (White, Rank 4), 2
            (White, Rank 5), 1
            (Yellow, Rank 1), 3
            (Yellow, Rank 2), 2
            (Yellow, Rank 3), 2
            (Yellow, Rank 4), 2
            (Yellow, Rank 5), 1
        ]
        |> List.sort
        |> Ok
    Game.prepareDrawDeck () |> Result.map (countBySuitAndRank >> List.sort) =! expectedCounts
