module HanabiGuru.Client.Console.Tests.DiscardCardIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Discarding a card increases the size of the discard pile`` (ValidNames names) =
    let game = CommandProcessingTestFramework.startGame names
    CommandProcessingTestFramework.giveInformation game
    |> Result.bind (CommandProcessingTestFramework.discardCard)
    |> Result.map (GameState.discard >> List.length) >! (GameState.discard game |> List.length |> Ok)
