module HanabiGuru.Engine.Tests.DeductionTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let candidateIdentities game =
    GameState.players game
    |> List.map (fun player ->
        let view = GameState.playerView player game
        PlayerView.hand view
        |> List.map (PlayerView.CardIdentity.deduce view))

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Each card always has a candidate identity for its true identity`` (GameInProgress game) =
    let trueIdentities =
        GameState.hands game
        |> List.collect (fun hand -> hand.cards |> List.map (fun { identity = card } -> card))

    candidateIdentities game
    |> List.collect id
    |> List.map (List.map (fun candidate -> candidate.card))
    |> List.map2 (fun trueIdentity -> List.filter ((=) trueIdentity)) trueIdentities
        =! (trueIdentities |> List.map List.singleton)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``All candidate identities always have a probability above zero and no greater than one`` (GameInProgress game) =
    candidateIdentities game
    |> List.collect id
    |> List.collect id
    |> List.filter (fun candidate -> candidate.probability <= 0.0 || candidate.probability > 1.0) =! []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The sum of probabilities for all candidates for each card is one`` (GameInProgress game) =
    test <@ candidateIdentities game
        |> List.map (List.map (List.sumBy (fun candidate -> candidate.probability)))
        |> List.collect id
        |> List.map ((-) 1.0 >> abs)
        |> List.forall ((>) 1e-10)
    @>

let private unrevealedCount player card game =
    GameState.hands game
    |> List.filter (fun hand -> hand.player = player)
    |> List.map (fun hand -> hand.cards |> List.map (fun { identity = card } -> card))
    |> List.exactlyOne
    |> List.append (GameState.drawDeck game)
    |> List.filter ((=) card)
    |> List.length

let private probabilitiesAndCounts game =
    GameState.players game
    |> List.map (fun player ->
        let view = GameState.playerView player game
        PlayerView.hand view
        |> List.map (fun card -> (player, PlayerView.CardIdentity.deduce view card)))
    |> List.collect (List.map (fun (player, candidates) ->
        candidates
        |> List.map (fun candidate -> (candidate, unrevealedCount player candidate.card game))))


[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Candidate identities exclude identities for which no card remains unrevealed``
    (GameInProgress game) =

    test <@ probabilitiesAndCounts game
        |> List.map (List.filter (snd >> ((>=) 0)))
        |> List.forall ((=) [])
    @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The probabilities of candidate identities must be proportional to the number of unrevealed instances``
    (GameInProgress game) =

    test <@ probabilitiesAndCounts game
        |> List.forall (fun candidates ->
            List.sortBy (fun ({ probability = p }, _) -> p) candidates
                = List.sortBy (fun (_, count) -> count) candidates)
    @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Candidate identities are returned in descending order of probability`` (GameInProgress game) =
    candidateIdentities game
    |> List.map (List.map (List.map (fun candidate -> candidate.probability)))
    |> List.forall (List.forall (fun probabilities -> probabilities = List.sortDescending probabilities))
