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

type CannotPlayCardReason =
    | CardNotInHand

type CannotPerformAction =
    | CannotAddPlayer of CannotAddPlayerReason list
    | CannotStartGame of CannotStartGameReason list
    | CannotTakeTurn of CannotTakeTurnReason list
    | CannotGiveInformation of CannotGiveInformationReason list
    | CannotDiscardCard of CannotDiscardCardReason list
    | CannotPlayCard of CannotPlayCardReason list

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

    let private nextTurnEvent game =
        GameAction.nextPlayer (GameState.players game) (GameState.activePlayer game |> Option.get) |> StartTurn

    let private turnEndEvents game =
        [nextTurnEvent game]
        |> (GameState.drawDeck game
            |> GameAction.draw
            |> Option.map (fun card -> CardDealtToPlayer (card, GameState.activePlayer game |> Option.get))
            |> Option.map (fun event -> fun events -> event :: events)
            |> Option.defaultValue id)

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
            @ [nextTurnEvent game]

        performPlayerTurn rules createEvents CannotGiveInformation game

    let discard (ConcealedCard cardKey) game =
        let rules =
            [
                AllClockTokensAvailable, GameState.clockTokens >> (=) GameRules.totalClockTokens
                CannotDiscardCardReason.CardNotInHand,
                    not << GameState.cardInHand (GameState.activePlayer game) cardKey
            ]

        let createEvents () = 
            CardDiscarded (GameState.card cardKey game |> Option.get)
            :: ClockTokenRestored
            :: turnEndEvents game

        performPlayerTurn rules createEvents CannotDiscardCard game

    let playCard (ConcealedCard cardKey) game =
        let rules =
            [
                CannotPlayCardReason.CardNotInHand,
                    not << GameState.cardInHand (GameState.activePlayer game) cardKey
            ]

        let createEvents () =
            let playEvent =
                let fireworks = GameState.fireworks game
                let alreadyPlayed card = List.contains card fireworks
                let startsFirework = (=) 1
                let continuesFirework suit rank = List.contains (Card (suit, Rank (rank - 1))) fireworks

                function
                | (Card (suit, Rank rank)) as card ->
                    if not <| alreadyPlayed card && (startsFirework rank || continuesFirework suit rank)
                    then CardAddedToFirework
                    else CardDiscarded

            let card = (GameState.card cardKey game |> Option.get)
            playEvent card.identity card
            :: turnEndEvents game

        performPlayerTurn rules createEvents CannotPlayCard game
