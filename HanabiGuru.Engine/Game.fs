namespace HanabiGuru.Engine

type CannotAddPlayerReason =
    | PlayerAlreadyJoined
    | NoSeatAvailable
    | GameAlreadyStarted

type CannotStartGameReason =
    | WaitingForMinimumPlayers
    | GameAlreadyStarted

type CannotPrepareTokensReason =
    | TokensAlreadyPrepared

type CannotPrepareDrawDeckReason =
    | DrawDeckAlreadyPrepared

type CannotDealCardReason =
    | DrawDeckEmpty

type CannotDealInitialHandsReason =
    | WaitingForMinimumPlayers
    | InsufficientCardsInDrawDeck
    | GameAlreadyStarted

type CannotAdvanceTurnReason =
    | GameNotStarted

type CannotPerformAction =
    | CannotAddPlayer of CannotAddPlayerReason list
    | CannotStartGame of CannotStartGameReason list
    | CannotPrepareTokens of CannotPrepareTokensReason list
    | CannotPrepareDrawDeck of CannotPrepareDrawDeckReason list
    | CannotDealCard of CannotDealCardReason list
    | CannotDealInitialHands of CannotDealInitialHandsReason list
    | CannotAdvanceTurn of CannotAdvanceTurnReason list

module Game =

    open HanabiGuru.Engine

    let private isPlayerJoined = function
        | PlayerJoined _ -> true
        | _ -> false

    let private isTokenAdded = function
        | FuseTokenAdded
        | ClockTokenAdded -> true
        | _ -> false

    let private isCardAddedToDrawDeck = function
        | CardAddedToDrawDeck _ -> true
        | _ -> false

    let private isCardDealtToPlayer = function
        | CardDealtToPlayer _ -> true
        | _ -> false

    let private isNextTurn = function
        | StartTurn _ -> true
        | _ -> false

    let private drawCard history = 
        let getCardAddedToDrawDeck = function
            | CardAddedToDrawDeck card -> Some card
            | _ -> None
        let getCardDealt = function
            | CardDealtToPlayer (card, _) -> Some card
            | _ -> None

        history
        |> EventHistory.choose getCardAddedToDrawDeck
        |> List.removeEach (EventHistory.choose getCardDealt history)
        |> List.randomItem Random.int

    let private canPerformAction history rules =
        rules
        |> List.filter (snd >> fun rule -> rule history)
        |> List.map fst

    let private performAction rules action createReasons history =
        match canPerformAction history rules with
        | [] -> action () |> EventHistory.recordEvents history |> Ok
        | reasons -> reasons |> createReasons |> Error

    let addPlayer player =
        let rules =
            [
                PlayerAlreadyJoined, EventHistory.contains (PlayerJoined player)
                NoSeatAvailable, EventHistory.countOf isPlayerJoined >> ((<=) GameRules.maximumPlayers)
                CannotAddPlayerReason.GameAlreadyStarted, EventHistory.exists (not << isPlayerJoined)
            ]

        let createEvents () = PlayerJoined player |> List.singleton

        performAction rules createEvents CannotAddPlayer

    let prepareTokens history =
        let rules = [ TokensAlreadyPrepared, EventHistory.exists isTokenAdded ]

        let createEvents () =
            [ (ClockTokenAdded, GameRules.clockTokensAvailable); (FuseTokenAdded, GameRules.fuseTokensAvailable) ]
            |> List.collect (fun (x, count) -> List.replicate count x)

        performAction rules createEvents CannotPrepareTokens history

    let prepareDrawDeck history =
        let rules = [ DrawDeckAlreadyPrepared, EventHistory.exists isCardAddedToDrawDeck ]

        let createEvents () =
            let suits = [Blue; Green; Red; White; Yellow]
            let ranks = [1; 1; 1; 2; 2; 3; 3; 4; 4; 5] |> List.map Rank
            suits
            |> List.collect (fun suit -> ranks |> List.map (fun rank -> suit, rank))
            |> List.map Card
            |> List.map CardAddedToDrawDeck

        performAction rules createEvents CannotPrepareDrawDeck history

    let dealCardToPlayer player history =
        let drawDeckIsEmpty history =
            EventHistory.countOf isCardAddedToDrawDeck history <= EventHistory.countOf isCardDealtToPlayer history
        let rules = [ DrawDeckEmpty, drawDeckIsEmpty ]

        let createEvents () = (drawCard history, player) |> CardDealtToPlayer |> List.singleton

        performAction rules createEvents CannotDealCard history

    let dealInitialHands history =
        let playerCount = EventHistory.countOf isPlayerJoined history
        let handSize = if playerCount <= 3 then 5 else 4
        let cardsRequired = playerCount * 5

        let rules =
            [
                WaitingForMinimumPlayers, EventHistory.countOf isPlayerJoined >> ((>) GameRules.minimumPlayers)
                InsufficientCardsInDrawDeck, EventHistory.countOf isCardAddedToDrawDeck >> ((>) cardsRequired)
                GameAlreadyStarted, EventHistory.exists isCardDealtToPlayer
            ]

        let createEvents () =
            history
            |> EventHistory.choose (function
                | PlayerJoined player -> Some player
                | _ -> None)
            |> List.replicate handSize
            |> List.collect id
            |> List.fold
                (fun (history, events) player ->
                    let event = (drawCard history, player) |> CardDealtToPlayer
                    EventHistory.recordEvent history event, event :: events)
                (history, [])
            |> snd
            |> List.rev
        
        performAction rules createEvents CannotDealInitialHands history

    let advanceTurn history =
        let getPlayerJoined = function
            | PlayerJoined player -> Some player
            | _ -> None

        let rules = [ GameNotStarted, not << EventHistory.exists isCardDealtToPlayer ]

        let createEvents () =
            Seq.initInfinite (fun _ -> EventHistory.choose getPlayerJoined history |> List.sort)
            |> Seq.collect id
            |> Seq.skip (EventHistory.countOf isNextTurn history)
            |> Seq.take 1
            |> Seq.map StartTurn
            |> List.ofSeq

        performAction rules createEvents CannotAdvanceTurn history

    let startGame history =
        let rules =
            [
                CannotStartGameReason.WaitingForMinimumPlayers, EventHistory.countOf isPlayerJoined >> ((>) GameRules.minimumPlayers)
                CannotStartGameReason.GameAlreadyStarted, EventHistory.exists (not << isPlayerJoined)
            ]

        let createEvents () =
            let suits = [Blue; Green; Red; White; Yellow]
            let ranks = [1; 1; 1; 2; 2; 3; 3; 4; 4; 5] |> List.map Rank
            let drawDeck =
                suits
                |> List.collect (fun suit -> ranks |> List.map (fun rank -> suit, rank))
                |> List.map Card
            let players = GameState.players history
            let cardsDealt = GameAction.dealInitialHands drawDeck players
            (drawDeck |> List.map CardAddedToDrawDeck) @ (cardsDealt |> List.map CardDealtToPlayer)

        performAction rules createEvents CannotStartGame history

        (*
        [prepareTokens; prepareDrawDeck; dealInitialHands; advanceTurn]
        |> List.fold (fun historyOrError step -> Result.bind step historyOrError) (Ok history)
        *)
