namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine

type TooManyPlayers = TooManyPlayers of Set<PlayerIdentity>
type GameReadyToStart = GameReadyToStart of EventHistory
type UpToThreePlayerGameInProgress = UpToThreePlayerGameInProgress of EventHistory
type FourOrMorePlayerGameInProgress = FourOrMorePlayerGameInProgress of EventHistory
type GameInProgress = GameInProgress of EventHistory
[<NoComparison>]
[<NoEquality>]
type GameInProgressAndNextTurn =
    GameInProgressAndNextTurn of EventHistory * (EventHistory -> Result<EventHistory, CannotPerformAction>)

type GameGeneration =
    static member private performAction game action =
        match action game with
        | Ok newGame -> newGame
        | Error _ -> game

    static member private addPlayers players =
        List.map Game.addPlayer players
        |> List.fold GameGeneration.performAction EventHistory.empty

    static member private generateGameReadyToStart minPlayers maxPlayers =
        Arb.generate<Set<PlayerIdentity>> 
        |> Gen.filter (Set.count >> ((<=) minPlayers)) 
        |> Gen.filter (Set.count >> ((>=) maxPlayers)) 
        |> Gen.map (Set.toList)
        |> Gen.map (fun players -> GameGeneration.addPlayers players)

    static member private generateStartedGame minPlayers maxPlayers =
        GameGeneration.generateGameReadyToStart minPlayers maxPlayers
        |> Gen.map (fun game -> GameGeneration.performAction game Game.startGame)

    static member private generateGameInProgress minPlayers maxPlayers =
        GameGeneration.generateStartedGame minPlayers maxPlayers
        |> Gen.map2 GameGeneration.turns (Gen.sized (fun s -> Gen.choose (0, s)))

    static member private generateTurn game =
        List.allPairs
            (GameState.players game |> Set.toList)
            [Blue; Green; Red; White; Yellow]
        |> List.map (fun (player, suit) -> Game.giveInformation player suit)
        |> List.choose (fun action ->
            match action game with
            | Ok newGame -> Some (action, newGame)
            | Error _ -> None)
        |> List.randomItem Random.int

    static member private turn = GameGeneration.generateTurn >> snd

    static member private turns n game =
        let rec takeTurn = function
            | (n, game) when n <= 0 -> game
            | (n, game) -> takeTurn (n - 1, GameGeneration.turn game)
        takeTurn (n, game)

    static member private toArb arbType = Gen.map arbType >> Arb.fromGen

    static member TooManyPlayers() =
        Arb.generate<Set<PlayerIdentity>>
        |> Gen.filter (Set.count >> ((<) GameRules.maximumPlayers))
        |> GameGeneration.toArb TooManyPlayers

    static member GameReadyToStart() =
        GameGeneration.generateGameReadyToStart GameRules.minimumPlayers GameRules.maximumPlayers
        |> GameGeneration.toArb GameReadyToStart

    static member UpToThreePlayerGameInProgress() =
        GameGeneration.generateGameInProgress GameRules.minimumPlayers 3
        |> GameGeneration.toArb UpToThreePlayerGameInProgress

    static member FourOrMorePlayerGameInProgress() =
        GameGeneration.generateGameInProgress 4 GameRules.maximumPlayers
        |> GameGeneration.toArb FourOrMorePlayerGameInProgress

    static member GameInProgress() =
        GameGeneration.generateGameInProgress GameRules.minimumPlayers GameRules.maximumPlayers
        |> GameGeneration.toArb GameInProgress

    static member GameInProgressAndNextTurn() =
        GameGeneration.generateGameInProgress GameRules.minimumPlayers GameRules.maximumPlayers
        |> Gen.map (fun game -> (game, GameGeneration.generateTurn game |> fst))
        |> GameGeneration.toArb GameInProgressAndNextTurn
