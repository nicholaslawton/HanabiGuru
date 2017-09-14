module HanabiGuru.Engine.Tests.GiveInformationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``After each player gives information to the next player, there are fewer candidate identities for each card``
    (GameInProgress game)
    (suit : Suit) =

    let players = GameState.players game |> Set.toList
    let candidateCards =
        DeductionTests.candidateIdentities >> List.collect (List.map (List.map (fun candidate -> candidate.card)))
    let giveInfoToNextPlayer game =
        let activePlayer = GameState.activePlayer game |> Option.get
        let playerView = GameState.playerView activePlayer game
        let nextPlayer = PlayerView.otherPlayers playerView |> List.head
        Game.giveInformation nextPlayer suit game

    let pairedCandidates =
        List.replicate (List.length players) giveInfoToNextPlayer
        |> List.fold GameAction.perform (Ok game)
        |> Result.map (candidateCards >> List.zip (candidateCards game))

    test <@ pairedCandidates
        |> Result.map (List.map (Pair.map List.length)
            >> List.forall (fun (initial, informed) -> informed < initial)) = Ok true @>

