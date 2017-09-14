module HanabiGuru.Engine.Tests.DeductionTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let candidateIdentities game =
    GameState.players game
    |> Set.toList
    |> List.map (fun player ->
        let view = GameState.playerView player game
        PlayerView.hand view
        |> List.map (PlayerView.CardIdentity.deduce view))

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Each card always has a candidate identity for its true identity`` (GameInProgress game) =
    let trueIdentities = GameState.hands game |> List.collect (fun hand -> hand.cards)

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

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Candidate identities include all identities for which at least one card remains unrevealed``
    (GameInProgress game) =

    let unrevealedCards =
        GameState.hands game
        |> List.map (fun hand ->
            hand.cards
            |> List.append (GameState.drawDeck game)
            |> List.replicate (List.length hand.cards)
            |> List.map set)

    candidateIdentities game
    |> List.map (List.map (List.map (fun candidate -> candidate.card) >> set)) =! unrevealedCards

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The probabilities of candidate identities are relative to the number of unrevealed instances``
    (GameInProgress game) =

    let unrevealedCards =
        GameState.hands game
        |> List.map (fun hand ->
            hand.cards
            |> List.append (GameState.drawDeck game)
            |> List.replicate (List.length hand.cards)
            |> List.map (List.countBy id
                >> List.sortByDescending (fun (card, count) -> (count, card))
                >> List.map fst))
    let sortCardsByProbability =
        List.sortByDescending (fun candidate -> (candidate.probability, candidate.card))
        >> List.map (fun candidate -> candidate.card)

    candidateIdentities game |> List.map (List.map sortCardsByProbability) =! unrevealedCards

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Candidate identities are returned in descending order of probability`` (GameInProgress game) =
    let candidateProbabilities =
        candidateIdentities game |> (List.map (List.map (List.map (fun candidate -> candidate.probability))))
    candidateProbabilities =! (candidateProbabilities |> List.map (List.sortDescending))
