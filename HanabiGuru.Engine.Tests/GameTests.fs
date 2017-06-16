module HanabiGuru.Engine.Tests.GameTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private performAction historyOrError action =
    let performActionAndUpdateHistory history =
        action history |> Result.map (List.fold EventHistory.recordEvent history)
    Result.bind performActionAndUpdateHistory historyOrError

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Can add a player who has not yet joined the game when there is a seat available``
    (CanAddPlayerArrangement (newPlayer, seatedPlayers)) =
    
    seatedPlayers
    |> List.map Game.addPlayer
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.bind (Game.addPlayer newPlayer) =! Ok [PlayerJoined newPlayer]

[<Property>]
let ``Adding a player repeatedly returns an error`` (player : Player) (PositiveInt repeats) =
    List.replicate repeats (Game.addPlayer player)
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.bind (Game.addPlayer player) =! Error (CannotAddPlayer [PlayerAlreadyJoined])

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Adding too many players returns an error`` (TooManyPlayers (newPlayer, seatedPlayers)) =
    seatedPlayers
    |> List.map Game.addPlayer
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.bind (Game.addPlayer newPlayer) =! Error (CannotAddPlayer [NoSeatAvailable])

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
    Game.prepareDrawDeck EventHistory.empty |> Result.map (countBySuitAndRank >> List.sort) =! expectedCounts

[<Property>]
let ``Preparing the draw deck repeatedly returns an error`` (history : EventHistory) (PositiveInt repeats) =
    List.replicate repeats Game.prepareDrawDeck
    |> List.fold performAction (Ok history)
    |> Result.bind Game.prepareDrawDeck =! Error (CannotPrepareDrawDeck [DrawDeckAlreadyPrepared])
