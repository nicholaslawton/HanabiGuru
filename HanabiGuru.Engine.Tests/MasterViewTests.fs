module HanabiGuru.Engine.Tests.MasterViewTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``After adding a player, the view contains one more player`` (view : MasterView) (player : Player) =
    MasterView.addPlayer view player
    |> fun v -> v.players
    |> List.length =! List.length view.players + 1

[<Property>]
let ``After adding a player, the view contains the added player`` (view : MasterView) (player : Player) =
    MasterView.addPlayer view player
    |> fun v -> v.players
    |> List.filter ((=) player)
    |> List.length >! 0

[<Property>]
let ``After adding a player, the players in the view are sorted in turn order`` (view : MasterView) (player : Player) =
    MasterView.addPlayer view player |> fun v -> v.players =! List.sort v.players

[<Property>]
let ``After adding a card to the draw deck, the view contains one more card`` (view : MasterView) (card : Card) =
    MasterView.addCardToDrawDeck view card
    |> fun v -> v.drawDeck
    |> List.length =! List.length view.drawDeck + 1

[<Property>]
let ``After adding a card to the draw deck, the view contains the added card`` (view : MasterView) (card : Card) =
    MasterView.addCardToDrawDeck view card
    |> fun v -> v.drawDeck
    |> List.filter ((=) card)
    |> List.length >! 0
