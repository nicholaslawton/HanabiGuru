module HanabiGuru.Engine.Tests.EventTests

open FsCheck
open FsCheck.Xunit

[<Property>]
let ``When a card is added to the draw deck, the draw deck contains the added card`` () =
    let addedCard = Card(Suit.Red, 2)
    let cardAdded = CardAddedToDrawDeck addedCard
    let gameState = GameState.Initial

    var newState = Engine.processEvent gameState cardAdded

    newState.drawDeck = [addedCard]
