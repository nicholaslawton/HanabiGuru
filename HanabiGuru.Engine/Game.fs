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
    | GameOver

type CannotGiveInformationReason =
    | NoClockTokensAvailable
    | NoMatchingCards
    | InvalidRecipient

type CannotDiscardCardReason =
    | AllClockTokensAvailable
    | CardNotInHand

type CannotPerformAction =
    | CannotAddPlayer of CannotAddPlayerReason list
    | CannotStartGame of CannotStartGameReason list
    | CannotTakeTurn of CannotTakeTurnReason list
    | CannotGiveInformation of CannotGiveInformationReason list
    | CannotDiscardCard of CannotDiscardCardReason list

module Game =

    open HanabiGuru.Engine

    let private isPlayerJoined = function
        | PlayerJoined _ -> true
        | _ -> false

    let private canPerformAction game rules =
        rules
        |> List.filter (snd >> fun rule -> rule game)
        |> List.map fst

    let private performAction rules action createReasons game =
        match canPerformAction game rules with
        | [] -> action () |> EventHistory.recordEvents game |> Ok
        | reasons -> reasons |> createReasons |> Error

    let private performPlayerTurn rules turn createReasons game =
        match GameState.state game with
        | GameState.NotStarted -> Error (CannotTakeTurn [GameNotStarted])
        | GameState.Finished -> Error (CannotTakeTurn [GameOver])
        | GameState.InProgress -> performAction rules turn createReasons game

    let addPlayer player =
        let rules =
            [
                PlayerAlreadyJoined, EventHistory.contains (PlayerJoined player)
                NoSeatAvailable, EventHistory.countOf isPlayerJoined >> ((<=) GameRules.maximumPlayers)
                CannotAddPlayerReason.GameAlreadyStarted, EventHistory.exists (not << isPlayerJoined)
            ]

        let createEvents () = PlayerJoined player |> List.singleton

        performAction rules createEvents CannotAddPlayer

    let startGame game =
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
            let players = GameState.players game
            let cardsDealt = GameAction.dealInitialHands drawDeck players
            let firstPlayer = List.head players

            (List.replicate GameRules.totalClockTokens ClockTokenAdded)
            @ (drawDeck |> List.map CardAddedToDrawDeck)
            @ (cardsDealt |> List.map CardDealtToPlayer)
            @ [StartTurn firstPlayer]

        performAction rules createEvents CannotStartGame game

    let giveInformation recipient cardTrait game =
        let info =
            GameState.hands game
            |> List.filter (fun hand -> hand.player = recipient)
            |> List.collect (fun hand -> hand.cards)
            |> List.map (GameAction.cardMatch cardTrait)

        let isMatch = function
            | CardInformation (_, Matches _) -> true
            | CardInformation (_, DoesNotMatch _) -> false

        let recipientIsSelf game = GameState.activePlayer game = Some recipient

        let recipientIsNotInGame = not << EventHistory.contains (PlayerJoined recipient)

        let noMatchingCards game =
            (info |> List.forall (not << isMatch)) && not (recipientIsSelf game)

        let rules =
            [
                NoClockTokensAvailable, GameState.clockTokens >> (=) 0
                NoMatchingCards, noMatchingCards
                InvalidRecipient, recipientIsSelf
                InvalidRecipient, recipientIsNotInGame
            ]

        let createEvents () =
            ClockTokenSpent
            :: (info |> List.map InformationGiven)
            @ (GameAction.nextPlayer
                (GameState.players game)
                (GameState.activePlayer game |> Option.get)
                |> StartTurn
                |> List.singleton)

        performPlayerTurn rules createEvents CannotGiveInformation game

    let discard (ConcealedCard cardKey) game =
        let activePlayer = GameState.activePlayer game
        let cardInHand =
            GameState.hands
            >> List.collect (fun hand -> List.map (fun card -> hand.player, card) hand.cards)
            >> List.exists (fun (owner, card) -> Some owner = activePlayer && card.instanceKey = cardKey)

        let rules =
            [
                AllClockTokensAvailable, GameState.clockTokens >> (=) GameRules.totalClockTokens
                CardNotInHand, not << cardInHand
            ]

        let createEvents () = 
            let activePlayer = Option.get activePlayer
            let drawDeck = GameState.drawDeck game

            let initialEvents =
                CardDiscarded (GameState.card cardKey game |> Option.get)
                :: ClockTokenRestored
                :: []
            let replacementDraw =
                if List.isEmpty drawDeck
                then List.empty
                else [CardDealtToPlayer (GameAction.draw drawDeck, activePlayer)]
            let finalEvents =
                GameAction.nextPlayer (GameState.players game) activePlayer
                |> StartTurn
                |> List.singleton

            List.collect id [initialEvents; replacementDraw; finalEvents]

        performPlayerTurn rules createEvents CannotDiscardCard game

    let play (ConcealedCard _cardKey) game = Ok game
