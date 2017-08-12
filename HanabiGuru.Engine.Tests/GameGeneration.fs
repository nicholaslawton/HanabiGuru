namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine

type GameReadyToStart = GameReadyToStart of EventHistory

type GameGeneration =
    static member GameReadyToStart() =
        let addPlayers players =
            let performAction game action =
                match action game with
                | Ok newGame -> newGame
                | Error _ -> game
            players
            |> List.map Game.addPlayer
            |> List.fold performAction EventHistory.empty

        Arb.generate<Set<PlayerIdentity>> 
        |> Gen.filter (Set.count >> ((<=) Game.minimumPlayers)) 
        |> Gen.filter (Set.count >> ((>=) Game.maximumPlayers)) 
        |> Gen.map (Set.toList >> addPlayers >> GameReadyToStart)
        |> Arb.fromGen
