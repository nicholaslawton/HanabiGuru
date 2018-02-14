module HanabiGuru.Client.Console.Tests.CommandTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open HanabiGuru.Client.Console

[<Property>]
let ``Selecting a card returns the identified card`` (cards : NonEmptyArray<ConcealedCard>) (i : int) =
    let cards = cards.Get |> List.ofArray |> List.truncate 26
    let identifiedCards = List.indexed cards |> List.map (Pair.mapFst Command.cardTag)
    let (c, card) = List.item (abs i % List.length identifiedCards) identifiedCards
    Command.selectCard c cards =! Ok card

[<Property>]
let ``Selecting an invalid card identifier returns an error`` (cards : ConcealedCard list) =
    Command.selectCard ' ' cards =! Error (InvalidCommand [InvalidCardTag])
