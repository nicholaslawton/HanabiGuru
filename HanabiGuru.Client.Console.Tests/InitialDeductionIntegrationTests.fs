module HanabiGuru.Client.Console.Tests.InitialDeductionIntegrationTests

open FsCheck.Xunit
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``After starting a game, players begin deducing the identities of their cards`` (Names names) =
    let startedGame = CommandProcessingTestFramework.startGame names
    GameState.players startedGame
    |> Set.toList
    |> List.map (fun player -> GameState.playerView player startedGame)
    |> List.map (fun view -> (PlayerView.hand view, view))
    |> List.map (fun (cards, view) -> List.map (PlayerView.CardIdentity.deduce view) cards)
    |> List.forall (not << List.isEmpty)
