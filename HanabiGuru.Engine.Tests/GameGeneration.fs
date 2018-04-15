namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine
open Pair
open Swensen.Unquote

type GiveInformationTurn = PlayerIdentity * CardTrait

type GameTurn =
    | GiveInformation of GiveInformationTurn
    | DiscardCard of ConcealedCard
    | PlayCard of ConcealedCard

type TurnClassification =
    | GiveInformation
    | DiscardCard
    | PlayCard

type TooManyPlayers = TooManyPlayers of Set<PlayerIdentity>
type GameReadyToStart = GameReadyToStart of EventHistory
type StartedGame = StartedGame of EventHistory
type UpToThreePlayerGameInProgress = UpToThreePlayerGameInProgress of EventHistory
type FourOrMorePlayerGameInProgress = FourOrMorePlayerGameInProgress of EventHistory
type GameInProgress = GameInProgress of EventHistory
type GameInProgressAndNextTurn = GameInProgressAndNextTurn of EventHistory * GameTurn
type GameInProgressAndGiveInformationTurn = GameInProgressAndGiveInformationTurn of EventHistory * GiveInformationTurn
type GameInProgressAndDiscardCardTurn = GameInProgressAndDiscardCardTurn of EventHistory * ConcealedCard
type GameInProgressAndPlayCardTurn = GameInProgressAndPlayCardTurn of EventHistory * ConcealedCard
type FinishedGame = FinishedGame of EventHistory
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

    static member private generateFinishedGame minPlayers maxPlayers =
        GameGeneration.generateStartedGame minPlayers maxPlayers
        |> Gen.map (GameGeneration.turnTimeline >> Seq.last >> fun (_, _, finishedGame) -> finishedGame)

    static member executeTurn game = function
        | GameTurn.GiveInformation (player, cardTrait) -> Game.giveInformation player cardTrait game
        | GameTurn.DiscardCard card -> Game.discard card game
        | GameTurn.PlayCard card -> Game.playCard card game

    static member private generateTurn game =
        let alreadyPlayedCards = GameState.fireworks game
        let playableCards =
            [1..5]
            |> Seq.map Rank
            |> Seq.allPairs Suit.allSuits
            |> Seq.map Card
            |> Seq.filter (fun card -> Seq.contains card alreadyPlayedCards |> not)
            |> Seq.groupBy (fun (Card (suit, _)) -> suit)
            |> Seq.map (Pair.mapSnd Seq.tryHead)
            |> Seq.choose snd

        let cardActionTurns action filter =
            GameState.activePlayer game
            |> Option.map (fun activePlayer ->
                GameState.hands game
                |> Seq.filter (fun hand -> hand.player = activePlayer)
                |> Seq.map (fun hand -> hand.cards)
                |> Seq.collect id)
            |> Option.map (Seq.filter filter)
            |> Option.map (Seq.map (fun card -> ConcealedCard card.instanceKey |> action))
            |> Option.defaultValue Seq.empty

        let anyCard _ = true
        let preferredCard preferredCards { identity = card } = Seq.contains card preferredCards
        let alreadyPlayedCard = preferredCard alreadyPlayedCards
        let playableCard = preferredCard playableCards

        let giveInformationTurns =
            seq [1..5]
            |> Seq.map (Rank >> RankTrait)
            |> Seq.append (seq Suit.allSuits |> Seq.map SuitTrait)
            |> Seq.allPairs (GameState.players game)
            |> Seq.map (fun (player, cardTrait) -> GameTurn.GiveInformation (player, cardTrait))
        let discardPreferredCardTurns = cardActionTurns GameTurn.DiscardCard alreadyPlayedCard
        let discardAnyCardTurns = cardActionTurns GameTurn.DiscardCard anyCard
        let playPreferredCardTurns = cardActionTurns GameTurn.PlayCard playableCard
        let playAnyCardTurns = cardActionTurns GameTurn.PlayCard anyCard

        [
            ( 9, giveInformationTurns)
            ( 7, discardPreferredCardTurns)
            ( 3, playPreferredCardTurns)
            ( 4, discardAnyCardTurns)
            ( 1, playAnyCardTurns)
        ]
        |> Seq.collect (fun (n, s) -> Seq.replicate n s |> Seq.collect id)
        |> Seq.sortBy (ignore >> Random.double)
        |> Seq.distinct
        |> Seq.tryPick (fun turn ->
            match GameGeneration.executeTurn game turn with
            | Ok newGame -> Some (turn, newGame)
            | Error _ -> None)

    static member private turnTimeline game =
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
        |> Seq.map (fun (game, (nextTurn, newGame)) -> (game, nextTurn, newGame))

    static member private turns lastTurnPredicate n game =
        GameGeneration.turnTimeline game
        |> Seq.findClosest lastTurnPredicate n
        |> Option.get
        |> fun (game, nextTurn, _) -> (game, nextTurn)

    static member private turnsUntil predicate game =
        GameGeneration.turnTimeline game
        |> Seq.find predicate

    static member private classifyTurn = function
        | GameTurn.GiveInformation _ -> TurnClassification.GiveInformation
        | GameTurn.DiscardCard _ -> TurnClassification.DiscardCard
        | GameTurn.PlayCard _ -> TurnClassification.PlayCard

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

    static member FinishedGame() =
        GameGeneration.generateFinishedGame GameRules.minimumPlayers GameRules.maximumPlayers
        |> GameGeneration.toArb FinishedGame

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
            (fun (_, turn, _) -> GameGeneration.classifyTurn turn = selectedTurn)
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
                | GameTurn.DiscardCard card -> card
                | _ -> new AssertionFailedException("Unexpected turn type") |> raise)

    static member GameInProgressAndPlayCardTurn() =
        GameGeneration.GameInProgressAndSelectedTurn
            GameInProgressAndPlayCardTurn
            PlayCard
            (function
                | GameTurn.PlayCard card -> card
                | _ -> new AssertionFailedException("Unexpected turn type") |> raise)
