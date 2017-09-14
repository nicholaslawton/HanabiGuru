namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine

type TooManyPlayers = TooManyPlayers of Set<PlayerIdentity>
type GameReadyToStart = GameReadyToStart of EventHistory
type UpToThreePlayerGameInProgress = UpToThreePlayerGameInProgress of EventHistory
type FourOrMorePlayerGameInProgress = FourOrMorePlayerGameInProgress of EventHistory
type GameInProgress = GameInProgress of EventHistory

type GameGeneration =
    static member private performAction game action =
        match action game with
        | Ok newGame -> newGame
        | Error _ -> game

    static member private playGame players =
        List.append (List.map Game.addPlayer players)
        >> List.fold GameGeneration.performAction EventHistory.empty

    static member private generateGameWithPlayerCount minPlayers maxPlayers actions =
        Arb.generate<Set<PlayerIdentity>> 
        |> Gen.filter (Set.count >> ((<=) minPlayers)) 
        |> Gen.filter (Set.count >> ((>=) maxPlayers)) 
        |> Gen.map (Set.toList)
        |> Gen.map (fun players -> GameGeneration.playGame players actions)

    static member private generateGame =
        GameGeneration.generateGameWithPlayerCount GameRules.minimumPlayers GameRules.maximumPlayers

    static member private toArb arbType = Gen.map arbType >> Arb.fromGen

    static member TooManyPlayers() =
        Arb.generate<Set<PlayerIdentity>>
        |> Gen.filter (Set.count >> ((<) GameRules.maximumPlayers))
        |> GameGeneration.toArb TooManyPlayers

    static member GameReadyToStart() =
        GameGeneration.generateGame []
        |> GameGeneration.toArb GameReadyToStart

    static member UpToThreePlayerGameInProgress() =
        [Game.startGame]
        |> GameGeneration.generateGameWithPlayerCount GameRules.minimumPlayers 3
        |> GameGeneration.toArb UpToThreePlayerGameInProgress

    static member FourOrMorePlayerGameInProgress() =
        [Game.startGame]
        |> GameGeneration.generateGameWithPlayerCount 4 GameRules.maximumPlayers
        |> GameGeneration.toArb FourOrMorePlayerGameInProgress

    static member GameInProgress() =
        [Game.startGame]
        |> GameGeneration.generateGame
        |> GameGeneration.toArb GameInProgress
