namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine

type GameReadyToStart = GameReadyToStart of EventHistory

type GameGeneration =
    static member GameReadyToStart() =
        let toGame players =
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
        |> fun gen -> (gen, (Arb.from<Set<PlayerIdentity>> |> Arb.toShrink)) 
        |> Arb.fromGenShrink 
        |> Arb.convert
            (Set.toList >> toGame >> GameReadyToStart)
            (fun (GameReadyToStart game) -> GameState.players game)
