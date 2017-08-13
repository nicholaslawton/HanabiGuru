﻿module HanabiGuru.Engine.Tests.GameTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private performAction historyOrError action = Result.bind action historyOrError

let private mapCannotPerformActionReasons fCannotAddPlayer fCannotStartGame = function
    | CannotAddPlayer reasons -> fCannotAddPlayer reasons 
    | CannotStartGame reasons -> fCannotStartGame reasons
    | _ -> []
 
let private selectReason reason = List.filter ((=) reason) 

let private selectNone _ = List.empty
 
let private selectCannotAddPlayerReason reason = 
    mapCannotPerformActionReasons (selectReason reason) selectNone
 
let private selectCannotStartGameReason reason = 
    mapCannotPerformActionReasons selectNone (selectReason reason)

[<Property>]
let ``Players can be added until the game is full`` (players : Set<PlayerIdentity>) =
    let players = players |> Set.toList |> List.truncate GameRules.maximumPlayers
    players
    |> List.map Game.addPlayer
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.map GameState.players =! Ok (set players)

[<Property>]
let ``Cannot add the same player more than once`` (player : PlayerIdentity) (PositiveInt repeats) =
    Game.addPlayer player
    |> List.replicate (1 + repeats)
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.mapError (selectCannotAddPlayerReason PlayerAlreadyJoined) =! Error [PlayerAlreadyJoined]

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Cannot add more than the maximum number of players`` (TooManyPlayers (newPlayer, seatedPlayers)) =
    newPlayer :: seatedPlayers
    |> List.rev
    |> List.map Game.addPlayer
    |> List.fold performAction (Ok EventHistory.empty) =! Error (CannotAddPlayer [NoSeatAvailable])

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot add a player after game has started`` (GameReadyToStart game) (player : PlayerIdentity) =
    Game.startGame :: [Game.addPlayer player]
    |> List.fold performAction (Ok game)
    |> Result.mapError (selectCannotAddPlayerReason CannotAddPlayerReason.GameAlreadyStarted)
        =! Error [CannotAddPlayerReason.GameAlreadyStarted]

[<Property>]
let ``Cannot start the game before the minimum number of players have joined`` (players : Set<PlayerIdentity>) =
    let addPlayers = players |> Set.toList |> List.truncate (GameRules.minimumPlayers - 1) |> List.map Game.addPlayer
    Game.startGame :: addPlayers
    |> List.rev
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.mapError (selectCannotStartGameReason CannotStartGameReason.WaitingForMinimumPlayers)
        =! Error [CannotStartGameReason.WaitingForMinimumPlayers]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game more than once returns an error`` (GameReadyToStart game) (PositiveInt repeats) =
    List.replicate (1 + repeats) Game.startGame
    |> List.fold performAction (Ok game)
    |> Result.mapError (selectCannotStartGameReason CannotStartGameReason.GameAlreadyStarted)
        =! Error [CannotStartGameReason.GameAlreadyStarted]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the fuse tokens to the game`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map GameState.fuseTokens =! Ok GameRules.fuseTokensAvailable

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the clock tokens to the game`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map GameState.clockTokens =! Ok GameRules.clockTokensAvailable

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game adds the standard set of cards to the game`` (GameReadyToStart game) =
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
        |> List.map (Pair.mapFst Card)
        |> List.sort

    let allCards game =
        let cardsInHands = GameState.hands game |> List.map (fun hand -> hand.hand)
        GameState.drawDeck game :: cardsInHands
        |> List.collect id

    Game.startGame game
    |> Result.map (allCards >> List.countBy id >> List.sort) =! Ok expectedCounts

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals five cards each for two or three players`` (UpToThreePlayerGameReadyToStart game) =
    Game.startGame game
    |> Result.map (GameState.hands >> List.map (fun hand -> hand.hand) >> List.map List.length)
        =! Ok (List.replicate (GameState.players game |> Set.count) 5)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals four cards each for four or five players`` (FourOrMorePlayerGameReadyToStart game) =
    Game.startGame game
    |> Result.map (GameState.hands >> List.map (fun hand -> hand.hand) >> List.map List.length)
        =! Ok (List.replicate (GameState.players game |> Set.count) 4)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game deals the initial hands non-deterministically`` (GameReadyToStart game) =
    List.replicate 5 game
    |> List.distinctBy Game.startGame
    |> List.length >! 1

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Starting the game starts the first turn`` (GameReadyToStart game) =
    let firstPlayer = game |> GameState.players |> Set.toList |> List.sort |> List.tryHead
    Game.startGame game
    |> Result.map GameState.activePlayer =! Ok firstPlayer

(*
let private appendHistory toEvent xs history = List.map toEvent xs |> List.fold EventHistory.recordEvent history

let private toHistory toEvent xs = appendHistory toEvent xs EventHistory.empty

let private performAction historyOrError action = Result.bind action historyOrError

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Can add a player who has not yet joined the game when there is a seat available``
    (CanAddPlayerArrangement (newPlayer, seatedPlayers)) =
    
    seatedPlayers
    |> List.map Game.addPlayer
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.bind (Game.addPlayer newPlayer) =! Ok [PlayerJoined newPlayer]

[<Property>]
let ``Adding a player repeatedly returns an error`` (player : PlayerIdentity) (PositiveInt repeats) =
    List.replicate repeats (Game.addPlayer player)
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.bind (Game.addPlayer player) =! Error (CannotAddPlayer [PlayerAlreadyJoined])

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Adding too many players returns an error`` (TooManyPlayers (newPlayer, seatedPlayers)) =
    seatedPlayers
    |> List.map Game.addPlayer
    |> List.fold performAction (Ok EventHistory.empty)
    |> Result.bind (Game.addPlayer newPlayer) =! Error (CannotAddPlayer [NoSeatAvailable])

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Cannot add a player after cards have been dealt``
    (TwoPlayers (seatedPlayer, newPlayer))
    (card : Card) =

    toHistory PlayerJoined [seatedPlayer]
    |> appendHistory GameEvent.CardAddedToDrawDeck [card]
    |> appendHistory GameEvent.CardDealtToPlayer [(card, seatedPlayer)]
    |> Game.addPlayer newPlayer =! Error (CannotAddPlayer [CannotAddPlayerReason.GameAlreadyStarted])
    
[<Property>]
let ``Preparing tokens returns events for adding initial fuse tokens`` (history : EventHistory) =
    history
    |> EventHistory.filter ((<>) GameEvent.FuseTokenAdded)
    |> EventHistory.filter ((<>) GameEvent.ClockTokenAdded)
    |> Game.prepareTokens 
    |> Result.map (List.filter ((=) GameEvent.FuseTokenAdded))
        =! (List.replicate Game.fuseTokensAvailable GameEvent.FuseTokenAdded |> Ok)
    
[<Property>]
let ``Preparing tokens returns events for adding initial clock tokens`` (history : EventHistory) =
    history
    |> EventHistory.filter ((<>) GameEvent.FuseTokenAdded)
    |> EventHistory.filter ((<>) GameEvent.ClockTokenAdded)
    |> Game.prepareTokens
    |> Result.map (List.filter ((=) GameEvent.ClockTokenAdded))
        =! (List.replicate Game.clockTokensAvailable GameEvent.ClockTokenAdded |> Ok)

[<Property>]
let ``Preparing tokens repeatedly returns an error`` (history : EventHistory) (PositiveInt repeats) =
    List.replicate (repeats + 1) Game.prepareTokens
    |> List.fold performAction (Ok history) =! Error (CannotPrepareTokens [TokensAlreadyPrepared])

[<Property>]
let ``Preparing the draw deck creates the events`` (history : EventHistory) =
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
    history
    |> EventHistory.filter (function | GameEvent.CardAddedToDrawDeck _ -> false | _ -> true)
    |> Game.prepareDrawDeck
    |> Result.map (countBySuitAndRank >> List.sort) =! expectedCounts

[<Property>]
let ``Preparing the draw deck repeatedly returns an error`` (history : EventHistory) (PositiveInt repeats) =
    List.replicate (repeats + 1) Game.prepareDrawDeck
    |> List.fold performAction (Ok history) =! Error (CannotPrepareDrawDeck [DrawDeckAlreadyPrepared])

[<Property>]
let ``Dealing all cards to a player creates an event for each card with the player as the recipient``
    (cards : Card list)
    (player : PlayerIdentity) =
 
    let expectedEvents = cards |> List.map (fun card -> CardDealtToPlayer (card, player))

    let cardDealtEvent = function
        | CardDealtToPlayer _ -> true
        | _ -> false

    List.replicate (List.length cards) (Game.dealCardToPlayer player)
    |> List.fold performAction (toHistory GameEvent.CardAddedToDrawDeck cards |> Ok)
    |> Result.map (EventHistory.filter cardDealtEvent >> EventHistory.events)
    |> Result.map List.sort =! (expectedEvents |> List.sort |> Ok)

[<Property>]
let ``Dealing more cards than are available returns an error`` (cards : Card list) (player : PlayerIdentity) =
    List.replicate (List.length cards + 1) (Game.dealCardToPlayer player)
    |> List.fold performAction (toHistory GameEvent.CardAddedToDrawDeck cards |> Ok)
        =! Error (CannotDealCard [DrawDeckEmpty])

[<Property>]
let ``Dealing a card repeatedly from the same draw deck does not deal the same card every time``
    (player : PlayerIdentity) =

    List.init 10 (fun rank -> Card (Blue, Rank rank))
    |> toHistory GameEvent.CardAddedToDrawDeck
    |> List.replicate 100
    |> List.map (Game.dealCardToPlayer player)
    |> Result.collect
    |> Result.map (List.countBy id)
    |> Result.map (List.filter (snd >> ((<) 30))) =! Ok []

let mapCannotDealInitialHandsReasons f =
    Result.mapError (function
        | CannotDealInitialHands reasons -> f reasons
        | _ -> [])

let filterCannotDealInitialHandsReason reason = mapCannotDealInitialHandsReasons (List.filter ((=) reason))

[<Property>]
let ``Dealing initial hands before at least two players have joined the game returns an error``
    (players : PlayerIdentity list) =

    players
    |> List.truncate 1
    |> toHistory PlayerJoined
    |> Game.dealInitialHands
    |> filterCannotDealInitialHandsReason WaitingForMinimumPlayers =! Error [WaitingForMinimumPlayers]

[<Property>]
let ``Dealing initial hands when there are insufficient cards in the draw deck returns an error``
    (players : NonEmptyArray<PlayerIdentity>)
    (cards : Card list) =

    let players = players.Get |> List.ofArray
    let handSize = if List.length players <= 3 then 5 else 4

    players
    |> toHistory PlayerJoined
    |> appendHistory GameEvent.CardAddedToDrawDeck (List.truncate (List.length players * handSize - 1) cards)
    |> Game.dealInitialHands
    |> filterCannotDealInitialHandsReason InsufficientCardsInDrawDeck =! Error [InsufficientCardsInDrawDeck]

[<Property>]
let ``Dealing initial hands repeatedly returns an error``
    (PositiveInt repeats)
    (cardsDealt : NonEmptyArray<Card * PlayerIdentity>) =

    List.replicate repeats Game.dealInitialHands
    |> List.fold performAction (cardsDealt.Get |> List.ofArray |> toHistory CardDealtToPlayer |> Ok)
    |> Result.bind Game.dealInitialHands
    |> filterCannotDealInitialHandsReason GameAlreadyStarted =! Error [GameAlreadyStarted]

let getCardDealt = function
    | CardDealtToPlayer (card, _) -> Some card
    | _ -> None

let getCardDealtRecipient = function
    | CardDealtToPlayer (_, player) -> Some player
    | _ -> None

[<Property>]
let ``Dealing initial hands deals five cards each for three or fewer players``
    (playerOne : PlayerIdentity)
    (playerTwo : PlayerIdentity)
    (playerThreeOrNothing : PlayerIdentity option)
    (card : Card) =

    let players = List.choose id [Some playerOne; Some playerTwo; playerThreeOrNothing]

    toHistory PlayerJoined players
    |> appendHistory GameEvent.CardAddedToDrawDeck (List.replicate (List.length players * 5) card)
    |> Game.dealInitialHands
    |> Result.map (List.choose getCardDealtRecipient) =! (List.replicate 5 players |> List.collect id |> Ok)

[<Property>]
let ``Dealing initial hands deals four cards each for four or more players``
    (playerOne : PlayerIdentity)
    (playerTwo : PlayerIdentity)
    (playerThree : PlayerIdentity)
    (playerFour : PlayerIdentity)
    (morePlayers : PlayerIdentity list)
    (card : Card) =

    let players = playerOne :: playerTwo :: playerThree :: playerFour :: morePlayers

    toHistory PlayerJoined players
    |> appendHistory GameEvent.CardAddedToDrawDeck (List.replicate (List.length players * 5) card)
    |> Game.dealInitialHands
    |> Result.map (List.choose getCardDealtRecipient) =! (List.replicate 4 players |> List.collect id |> Ok)

type TenOrMoreCards = TenOrMoreCards of Card list

type MinDrawDeck =
    static member TenOrMoreCards() =
        Arb.generate<Card>
        |> Gen.nonEmptyListOf 
        |> Gen.filter (List.length >> ((<=) 10)) 
        |> Gen.map TenOrMoreCards
        |> Arb.fromGen

[<Property(Arbitrary = [| typeof<MinDrawDeck> |])>]
let ``Dealing initial hands deals cards from the draw deck, leaving the excess``
    (TenOrMoreCards cards)
    (playerOne : PlayerIdentity)
    (playerTwo : PlayerIdentity) =

    let cardsDealtOrError =
        toHistory PlayerJoined [playerOne; playerTwo]
        |> appendHistory GameEvent.CardAddedToDrawDeck cards
        |> Game.dealInitialHands
        |> Result.map (List.choose getCardDealt)

    let cardsNotDealtOrError =
        cardsDealtOrError
        |> Result.map (fun cardsDealt -> List.removeEach cardsDealt cards)

    Result.combine (@) cardsDealtOrError cardsNotDealtOrError
    |> Result.map List.sort =! (cards |> List.sort |> Ok)

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``Advancing the turn returns an event for the turn of the next player``
    (Players players)
    (card : Card)
    (PositiveInt currentTurnNumber) =

    let turns = Seq.initInfinite (fun _ -> List.sort players) |> Seq.collect id

    (toHistory PlayerJoined players)
    |> appendHistory CardDealtToPlayer [(card, List.head players)]
    |> appendHistory StartTurn (turns |> Seq.take currentTurnNumber |> List.ofSeq)
    |> Game.advanceTurn =! Ok (turns |> Seq.skip currentTurnNumber |> Seq.head |> StartTurn |> List.singleton)

[<Property>]
let ``Advancing to the first turn returns an event for the turn of the first player``
    (players : NonEmptyArray<PlayerIdentity>)
    (card : Card) =

    let players = players.Get |> List.ofArray
    (toHistory PlayerJoined players)
    |> appendHistory CardDealtToPlayer [(card, List.head players)]
    |> Game.advanceTurn =! Ok (players |> List.sort |> List.head |> StartTurn |> List.singleton)

[<Property>]
let ``Advancing the turn returns an error when the game has not started`` (history : EventHistory) =
    history
    |> EventHistory.filter (function
        | CardDealtToPlayer _ -> false
        | _ -> true)
    |> Game.advanceTurn =! Error (CannotAdvanceTurn [GameNotStarted])
    *)
