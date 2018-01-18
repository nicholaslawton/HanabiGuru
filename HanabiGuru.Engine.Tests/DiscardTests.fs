module HanabiGuru.Engine.Tests.DiscardTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Discarding a card restores a clock token``
    (GameInProgressAndDiscardCardTurn (game, card)) =

    Game.discard card game
    |> Result.map (GameState.clockTokens) =! Ok (GameState.clockTokens game + 1)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Discarding a card adds it to the discard pile``
    (GameInProgressAndDiscardCardTurn (game, (ConcealedCard cardKey))) =

    let card = (GameState.card cardKey game |> Option.get).identity
    let matchingCardsInDiscard = GameState.discard >> List.filter ((=) card)

    Game.discard (ConcealedCard cardKey) game
    |> Result.map matchingCardsInDiscard >=! Ok (card :: matchingCardsInDiscard game)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``After discarding a card, the player draws a replacement card from the deck if not empty``
    (GameInProgressAndDiscardCardTurn (game, card)) =

    Game.discard card game
    |> Result.map (GameState.drawDeck >> List.length) =! Ok ((GameState.drawDeck game |> List.length) - 1 |> max 0)

let private select reason = function
    | CannotDiscardCard reasons -> List.filter ((=) reason) reasons
    | _ -> []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Discarding cards repeatedly fails once there are no clock tokens available to recover`` (GameInProgress game) =
    let discardFirstCard game =
        let activePlayer = GameState.activePlayer game |> Option.get
        let firstCard =
            GameState.playerView activePlayer game
            |> PlayerView.hand
            |> List.head
        Game.discard firstCard game

    let result =
        List.replicate (GameRules.totalClockTokens + 1) discardFirstCard
        |> List.fold GameAction.perform (Ok game)

    test <@
            result = Error (CannotTakeTurn [GameOver]) // if the game ends first, that's okay...
            || result |> Result.mapError (select CannotDiscardCardReason.AllClockTokensAvailable)
                    = Error [CannotDiscardCardReason.AllClockTokensAvailable] @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Discarding a card from the hand of another player is not permitted`` (GameInProgress game) =
    let activePlayer = GameState.activePlayer game |> Option.get
    let view = GameState.playerView activePlayer game
    view
    |> PlayerView.otherPlayers
    |> List.head
    |> fun otherPlayer -> (PlayerView.otherHand otherPlayer view).cards
    |> List.head
    |> fun otherCard -> Game.discard (ConcealedCard otherCard.instanceKey) game
    |> Result.mapError (select CannotDiscardCardReason.CardNotInHand)
        =! Error [CannotDiscardCardReason.CardNotInHand]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Discarding a card not in the game is not permitted`` (GameInProgress game) (card : ConcealedCard) =
    Game.discard card game
    |> Result.mapError (select CannotDiscardCardReason.CardNotInHand)
        =! Error [CannotDiscardCardReason.CardNotInHand]
