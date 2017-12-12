namespace HanabiGuru.Engine

type CannotAddPlayerReason =
    | PlayerAlreadyJoined
    | NoSeatAvailable
    | GameAlreadyStarted

type CannotStartGameReason =
    | WaitingForMinimumPlayers
    | GameAlreadyStarted

type CannotTakeTurnReason =
    | GameNotStarted

type CannotGiveInformationReason =
    | NoClockTokensAvailable
    | NoMatchingCards
    | InvalidRecipient

type CannotPerformAction =
    | CannotAddPlayer of CannotAddPlayerReason list
    | CannotStartGame of CannotStartGameReason list
    | CannotTakeTurn of CannotTakeTurnReason list
    | CannotGiveInformation of CannotGiveInformationReason list

module Game =

    open HanabiGuru.Engine

    let private isPlayerJoined = function
        | PlayerJoined _ -> true
        | _ -> false

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

    let startGame history =
        let rules =
            [
                WaitingForMinimumPlayers, EventHistory.countOf isPlayerJoined >> ((>) GameRules.minimumPlayers)
                GameAlreadyStarted, EventHistory.exists (not << isPlayerJoined)
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
            let firstPlayer = List.head players

            (List.replicate GameRules.clockTokensAvailable ClockTokenAdded)
            @ (drawDeck |> List.map CardAddedToDrawDeck)
            @ (cardsDealt |> List.map CardDealtToPlayer)
            @ [StartTurn firstPlayer]

        performAction rules createEvents CannotStartGame history

    let giveInformation recipient cardTrait history =
        let determineInfo =
            GameState.hands
            >> List.filter (fun hand -> hand.player = recipient)
            >> List.collect (fun hand -> hand.cards)
            >> List.map (GameAction.cardMatch cardTrait)

        let isMatch = function
            | CardInformation (_, Matches _) -> true
            | CardInformation (_, DoesNotMatch _) -> false

        let recipientIsSelf history = GameState.activePlayer history = Some recipient

        let recipientIsNotInGame = GameState.players >> List.contains recipient >> not

        let noMatchingCards history =
            (determineInfo history |> List.forall (not << isMatch)) && not (recipientIsSelf history)

        let rules =
            [
                NoClockTokensAvailable, GameState.clockTokens >> (=) 0
                NoMatchingCards, noMatchingCards
                InvalidRecipient, recipientIsSelf
                InvalidRecipient, recipientIsNotInGame
            ]

        let createEvents () =
            (GameAction.nextPlayer
                (GameState.players history)
                (GameState.activePlayer history |> Option.get)
                |> StartTurn)
            :: ClockTokenSpent
            :: (determineInfo history |> List.map InformationGiven)

        if GameState.activePlayer history = None
        then Error (CannotTakeTurn [GameNotStarted])
        else performAction rules createEvents CannotGiveInformation history
