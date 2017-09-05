module HanabiGuru.Client.Console.Tests.InitialDeductionIntegrationTests

open FsCheck.Xunit
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<InputGeneration> |])>]
let ``After starting a game, players begin deducing the identities of their cards`` (Names names) =
    let startedGame = CommandProcessingTestFramework.startGame names
    GameState.players startedGame
    |> Set.toList
    |> List.map (fun player ->
        let view = GameState.playerView player startedGame
        PlayerView.hand view
        |> List.map (PlayerView.CardIdentity.deduce view))
    |> List.forall (not << List.isEmpty)
