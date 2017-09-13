module HanabiGuru.Engine.Tests.GiveInformationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``After each player gives information to the next player, there are fewer candidate identities for each card``
    (GameReadyToStart game)
    (suit : Suit) =

    let startedGame = Game.startGame game
    let players = GameState.players game |> Set.toList
    let candidateIdentities game =
        players
        |> List.map (fun player ->
            let view = GameState.playerView player game
            PlayerView.hand view
            |> List.map (PlayerView.CardIdentity.deduce view))
    let candidateCards = candidateIdentities >> List.collect id >> List.map (List.map (fun candidate -> candidate.card))
    let giveInfoToNextPlayer game =
        let activePlayer = GameState.activePlayer game |> Option.get
        let playerView = GameState.playerView activePlayer game
        let nextPlayer = PlayerView.otherPlayers playerView |> List.head
        Game.giveInformation nextPlayer suit game

    let pairedCandidates =
        List.replicate (List.length players) giveInfoToNextPlayer
        |> List.fold GameAction.perform startedGame
        |> Result.map candidateCards
        |> Result.bind (fun candidates ->
            startedGame
            |> Result.map candidateCards
            |> Result.map (fun initialCandidates ->
                candidates
                |> List.zip initialCandidates))

    test <@ pairedCandidates |> Result.map (List.forall (fun (initial, informed) -> informed < initial)) = Ok true @>
