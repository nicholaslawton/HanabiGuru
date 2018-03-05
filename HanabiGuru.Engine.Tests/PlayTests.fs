﻿module HanabiGuru.Engine.Tests.PlayTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Playing a card adds it to the fireworks display or the discard pile``
    (GameInProgressAndPlayCardTurn (game, (ConcealedCard cardKey))) =

    let card = (GameState.card cardKey game |> Option.get).identity
    let matchingCardsInFireworksAndDiscard game =
        GameState.fireworks game @ GameState.discard game |> List.filter ((=) card)

    Game.playCard (ConcealedCard cardKey) game
    |> Result.map matchingCardsInFireworksAndDiscard =! Ok (card :: matchingCardsInFireworksAndDiscard game)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The played card is added to the fireworks display if it begins, adds to or completes a firework``
    (GameInProgressAndPlayCardTurn (game, (ConcealedCard cardKey))) =

    let card = (GameState.card cardKey game |> Option.get).identity
    let playable (Card (suit, Rank rank)) game =
        rank =
            (GameState.fireworks game
                |> List.filter (fun (Card (s, _)) -> s = suit)
                |> List.length) + 1

    let minExpectedFireworksAfter =
        (GameState.fireworks game |> List.length) + (if playable card game then 1 else 0)
    
    Game.playCard (ConcealedCard cardKey) game
    |> Result.map (GameState.fireworks >> List.length) >=! Ok minExpectedFireworksAfter

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``After playing a card, the player draws a replacement card from the deck if not empty``
    (GameInProgressAndDiscardCardTurn (game, card)) =

    Game.playCard card game
    |> Result.map (GameState.drawDeck >> List.length) =! Ok ((GameState.drawDeck game |> List.length) - 1 |> max 0)

let private select reason = function
    | CannotPlayCard reasons -> List.filter ((=) reason) reasons
    | _ -> []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Playing a card from the hand of another player is not permitted`` (GameInProgress game) =
    let activePlayer = GameState.activePlayer game |> Option.get
    let view = GameState.playerView activePlayer game
    view
    |> PlayerView.otherPlayers
    |> List.head
    |> fun otherPlayer -> (PlayerView.otherHand otherPlayer view).cards
    |> List.head
    |> fun otherCard -> Game.playCard (ConcealedCard otherCard.instanceKey) game
    |> Result.mapError (select CannotPlayCardReason.CardNotInHand)
        =! Error [CannotPlayCardReason.CardNotInHand]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Playing a card not in the game is not permitted`` (GameInProgress game) (card : ConcealedCard) =
    Game.playCard card game
    |> Result.mapError (select CannotPlayCardReason.CardNotInHand)
        =! Error [CannotPlayCardReason.CardNotInHand]
