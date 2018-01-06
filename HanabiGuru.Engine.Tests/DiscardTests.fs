module HanabiGuru.Engine.Tests.DiscardTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Discarding a card restores a clock token``
    (GameInProgressAndDiscardCardTurn (game, card)) =

    Game.discard card game
    |> Result.map (GameState.clockTokens) =! Ok (GameState.clockTokens game + 1)

let private select reason = function
    | CannotDiscardCard reasons -> List.filter ((=) reason) reasons
    | _ -> []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Discarding cards repeatedly fails once there are no clock tokens available to recover`` (GameInProgress game) =
    Game.discard ()
    |> List.replicate (GameRules.totalClockTokens + 1)
    |> List.fold GameAction.perform (Ok game)
    |> Result.mapError (select CannotDiscardCardReason.AllClockTokensAvailable)
        =! Error [CannotDiscardCardReason.AllClockTokensAvailable]
