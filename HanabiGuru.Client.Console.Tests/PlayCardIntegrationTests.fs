module HanabiGuru.Client.Console.Tests.PlayCardIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``Playing a card increases the size of the fireworks display or discard pile`` (ValidNames names) =
    let game = CommandProcessingTestFramework.startGame names
    let fireworksAndDiscard game = GameState.fireworks game @ GameState.discard game
    CommandProcessingTestFramework.playCard game
    |> Result.map (fireworksAndDiscard >> List.length) >! (fireworksAndDiscard game |> List.length |> Ok)

