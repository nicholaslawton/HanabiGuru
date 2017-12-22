namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine
open Pair
open Swensen.Unquote

type GiveInformationTurn = PlayerIdentity * CardTrait
type DiscardCardTurn = ConcealedCard

type GameTurn =
    | GiveInformation of GiveInformationTurn
    | DiscardCard of DiscardCardTurn

type TurnClassification =
    | GiveInformation
    | DiscardCard

type TooManyPlayers = TooManyPlayers of Set<PlayerIdentity>
type GameReadyToStart = GameReadyToStart of EventHistory
type StartedGame = StartedGame of EventHistory
type UpToThreePlayerGameInProgress = UpToThreePlayerGameInProgress of EventHistory
type FourOrMorePlayerGameInProgress = FourOrMorePlayerGameInProgress of EventHistory
type GameInProgress = GameInProgress of EventHistory
type GameInProgressAndNextTurn = GameInProgressAndNextTurn of EventHistory * GameTurn
type GameInProgressAndGiveInformationTurn = GameInProgressAndGiveInformationTurn of EventHistory * GiveInformationTurn
type GameInProgressAndDiscardCardTurn = GameInProgressAndDiscardCardTurn of EventHistory * DiscardCardTurn
type PlayerTurn = PlayerTurn of GameTurn

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
        |> Gen.map (Set.toList >> List.sortBy (ignore >> Random.double))
        |> Gen.map (fun players -> GameGeneration.addPlayers players)

    static member private generateStartedGame minPlayers maxPlayers =
        GameGeneration.generateGameReadyToStart minPlayers maxPlayers
        |> Gen.map (fun game -> GameGeneration.performAction game Game.startGame)

    static member private generateGameInProgress minPlayers maxPlayers =
        GameGeneration.generateGameInProgressAndNextTurn minPlayers maxPlayers (fun _ -> true)
        |> Gen.map fst

    static member private generateGameInProgressAndNextTurn minPlayers maxPlayers nextTurnPredicate =
        GameGeneration.generateStartedGame minPlayers maxPlayers
        |> Gen.map2 (GameGeneration.turns nextTurnPredicate) (Gen.sized (fun s -> Gen.choose (0, max 0 s)))

    static member executeTurn game = function
        | GameTurn.GiveInformation (player, cardTrait) -> Game.giveInformation player cardTrait game
        | GameTurn.DiscardCard card -> Game.discard card game

    static member private generateTurn game =
        let giveInformationTurns =
            seq [1..5]
            |> Seq.map (Rank >> RankTrait)
            |> Seq.append (seq [Blue; Green; Red; White; Yellow] |> Seq.map SuitTrait)
            |> Seq.allPairs (GameState.players game)
            |> Seq.map (fun (player, cardTrait) -> GameTurn.GiveInformation (player, cardTrait))
        let discardCardTurns =
            GameState.activePlayer game
            |> Option.map (fun activePlayer ->
                GameState.playerView activePlayer game
                |> PlayerView.hand
                |> Seq.map GameTurn.DiscardCard)
            |> Option.defaultValue Seq.empty

        Seq.concat (seq [giveInformationTurns; discardCardTurns])
        |> Seq.sortBy (ignore >> Random.double)
        |> Seq.tryPick (fun turn ->
            match GameGeneration.executeTurn game turn with
            | Ok newGame -> Some (turn, newGame)
            | Error _ -> None)

    static member private turns lastTurnPredicate n game =
        let timeline =
            Seq.initInfinite id
            |> Seq.scan
                (fun (_, previousTurnAndCurrentGameOrNothing) _ ->
                    let currentGame = 
                        match previousTurnAndCurrentGameOrNothing with
                        | None -> game
                        | Some (_, currentGame) -> currentGame
                    (currentGame, GameGeneration.generateTurn currentGame))
                (game, None)
            |> Seq.skip 1
            |> Seq.takeWhile (fun (_, o) -> o <> None)
            |> Seq.map (mapSnd Option.get)
            |> Seq.map (fun (game, (nextTurn, _)) -> (game, nextTurn))
            
        timeline
        |> Seq.truncate n
        |> Seq.tryFindBack (snd >> lastTurnPredicate)
        |> Option.map (fun x -> lazy x)
        |> Option.defaultValue (lazy (timeline |> Seq.skip n |> Seq.find (snd >> lastTurnPredicate)))
        |> fun l -> l.Value

    static member private classifyTurn = function
        | GameTurn.GiveInformation _ -> TurnClassification.GiveInformation
        | GameTurn.DiscardCard _ -> TurnClassification.DiscardCard

    static member private toArb arbType = Gen.map arbType >> Arb.fromGen

    static member TooManyPlayers() =
        Arb.generate<Set<PlayerIdentity>>
        |> Gen.filter (Set.count >> ((<) GameRules.maximumPlayers))
        |> GameGeneration.toArb TooManyPlayers

    static member GameReadyToStart() =
        GameGeneration.generateGameReadyToStart GameRules.minimumPlayers GameRules.maximumPlayers
        |> GameGeneration.toArb GameReadyToStart

    static member StartedGame() =
        GameGeneration.generateStartedGame GameRules.minimumPlayers GameRules.maximumPlayers
        |> GameGeneration.toArb StartedGame

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
        GameGeneration.generateGameInProgressAndNextTurn
            GameRules.minimumPlayers
            GameRules.maximumPlayers
            (fun _ -> true)
        |> GameGeneration.toArb GameInProgressAndNextTurn

    static member private GameInProgressAndSelectedTurn arbType selectedTurn extractTurn =
        GameGeneration.generateGameInProgressAndNextTurn
            GameRules.minimumPlayers
            GameRules.maximumPlayers
            (GameGeneration.classifyTurn >> ((=) selectedTurn))
        |> Gen.map (mapSnd extractTurn)
        |> GameGeneration.toArb arbType

    static member GameInProgressAndGiveInformationTurn() =
        GameGeneration.GameInProgressAndSelectedTurn
            GameInProgressAndGiveInformationTurn
            GiveInformation
            (function
                | GameTurn.GiveInformation info -> info
                | _ -> new AssertionFailedException("Unexpected turn type") |> raise)

    static member GameInProgressAndDiscardCardTurn() =
        GameGeneration.GameInProgressAndSelectedTurn
            GameInProgressAndDiscardCardTurn
            DiscardCard
            (function
                | GameTurn.DiscardCard info -> info
                | _ -> new AssertionFailedException("Unexpected turn type") |> raise)
