namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine

type TooManyPlayers = TooManyPlayers of Set<PlayerIdentity>
type GameReadyToStart = GameReadyToStart of EventHistory
type UpToThreePlayerGameReadyToStart = UpToThreePlayerGameReadyToStart of EventHistory
type FourOrMorePlayerGameReadyToStart = FourOrMorePlayerGameReadyToStart of EventHistory

type GameGeneration =
    static member private gameReadyToStart minPlayers maxPlayers =
        let addPlayers players =
            let performAction game action =
                match action game with
                | Ok newGame -> newGame
                | Error _ -> game
            players
            |> List.map Game.addPlayer
            |> List.fold performAction EventHistory.empty

        Arb.generate<Set<PlayerIdentity>> 
        |> Gen.filter (Set.count >> ((<=) minPlayers)) 
        |> Gen.filter (Set.count >> ((>=) maxPlayers)) 
        |> Gen.map (Set.toList >> addPlayers)
    
    static member private toArb arbType = Gen.map arbType >> Arb.fromGen

    static member TooManyPlayers() =
        Arb.generate<Set<PlayerIdentity>>
        |> Gen.filter (Set.count >> ((<) GameRules.maximumPlayers))
        |> GameGeneration.toArb TooManyPlayers

    static member GameReadyToStart() =
        GameGeneration.gameReadyToStart GameRules.minimumPlayers GameRules.maximumPlayers
        |> GameGeneration.toArb GameReadyToStart

    static member UpToThreePlayerGameReadyToStart() =
        GameGeneration.gameReadyToStart GameRules.minimumPlayers 3
        |> GameGeneration.toArb UpToThreePlayerGameReadyToStart

    static member FourOrMorePlayerGameReadyToStart() =
        GameGeneration.gameReadyToStart 4 GameRules.maximumPlayers
        |> GameGeneration.toArb FourOrMorePlayerGameReadyToStart
