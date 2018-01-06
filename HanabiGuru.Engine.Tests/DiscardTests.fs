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
let ``Discarding a card removes it from the hand of the active player``
    (GameInProgressAndDiscardCardTurn (game, (ConcealedCard cardKey))) =

    let activePlayer = GameState.activePlayer game |> Option.get
    let cardsInHand game = (GameState.hands game |> List.find (fun hand -> hand.player = activePlayer)).cards

    Game.discard (ConcealedCard cardKey) game
    |> Result.map (cardsInHand >> List.filter (fun card -> card.instanceKey = cardKey)) =! Ok ([])

let private select reason = function
    | CannotDiscardCard reasons -> List.filter ((=) reason) reasons
    | _ -> []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Discarding cards repeatedly fails once there are no clock tokens available to recover``
    (GameInProgress game)
    (card : ConcealedCard) =

    Game.discard card
    |> List.replicate (GameRules.totalClockTokens + 1)
    |> List.fold GameAction.perform (Ok game)
    |> Result.mapError (select CannotDiscardCardReason.AllClockTokensAvailable)
        =! Error [CannotDiscardCardReason.AllClockTokensAvailable]
