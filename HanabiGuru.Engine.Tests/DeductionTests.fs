module HanabiGuru.Engine.Tests.DeductionTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let candidateIdentities game =
    GameState.players game
    |> List.collect (fun player ->
        let view = GameState.playerView player game
        PlayerView.hand view
        |> List.collect (fun (ConcealedCard cardKey) ->
            PlayerView.CardIdentity.deduce view (ConcealedCard cardKey)
            |> List.map (fun candidateIdentity -> (player, cardKey, candidateIdentity))))

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Each card always has a candidate identity for its true identity`` (GameInProgress game) =
    let trueIdentities =
        GameState.hands game
        |> List.collect (fun hand ->
            hand.cards |> List.map (fun card ->
                (hand.player, card.instanceKey, card.identity)))
    let candidates =
        candidateIdentities game
        |> List.map (fun (player, key, candidate) -> (player, key, candidate.card))
    
    trueIdentities |> List.filter (fun x -> not <| List.contains x candidates) =! []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``All candidate identities always have a probability above zero and no greater than one`` (GameInProgress game) =
    candidateIdentities game
    |> List.filter (fun (_, _, candidate) -> candidate.probability <= 0.0 || candidate.probability > 1.0) =! []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The sum of probabilities for all candidates for each card is one`` (GameInProgress game) =
    test <@ candidateIdentities game
        |> List.groupBy (fun (_, key, _) -> key)
        |> List.map (Pair.mapSnd (List.sumBy (fun (_, _, candidate) -> candidate.probability)))
        |> List.map (snd >> (-) 1.0 >> abs)
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
    test <@ candidateIdentities game
        |> List.groupBy (fun (_, key, _) -> key)
        |> List.map (Pair.mapSnd (List.map (fun (_, _, candidate) -> candidate.probability)))
        |> List.forall (snd >> fun probabilities -> probabilities = List.sortDescending probabilities) @>
