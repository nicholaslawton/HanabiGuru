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
let ``There is always at least one candidate identity for all cards`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map candidateIdentities
    |> Result.map (fun candidates ->
        candidates
        |> List.collect id
        |> List.filter List.isEmpty) =! Ok []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``All candidate identities always have a probability above zero and no greater than one`` (GameReadyToStart game) =
    Game.startGame game
    |> Result.map candidateIdentities
    |> Result.map (fun candidates ->
        candidates
        |> List.collect id
        |> List.collect id
        |> List.filter (fun candidate -> candidate.probability <= 0.0 || candidate.probability > 1.0)) =! Ok []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The sum of probabilities for all candidates for each card is one`` (GameReadyToStart game) =
    test <@ Game.startGame game
        |> Result.map (candidateIdentities
            >> List.map (List.map (List.sumBy (fun candidate -> candidate.probability)))
            >> List.collect id
            >> List.map ((-) 1.0 >> abs)
            >> List.forall ((>) 1e-10)) = Ok true
    @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Candidate identities include all identities for which at least one card remains unrevealed``
    (GameReadyToStart game) =

    let startedGameOrError = Game.startGame game
    let unrevealedCards =
        startedGameOrError
        |> Result.map (fun startedGame ->
            GameState.hands startedGame
            |> List.map (fun hand ->
                    hand.cards
                    |> List.append (GameState.drawDeck startedGame)
                    |> List.replicate (List.length hand.cards)
                    |> List.map set))

    startedGameOrError
    |> Result.map (candidateIdentities >> List.map (List.map (List.map (fun candidate -> candidate.card) >> set)))
        =! unrevealedCards

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The probabilities of candidate identities are relative to the number of unrevealed instances``
    (GameReadyToStart game) =

    let startedGameOrError = Game.startGame game
    let unrevealedCards =
        startedGameOrError
        |> Result.map (fun startedGame ->
            GameState.hands startedGame
            |> List.map (fun hand ->
                hand.cards
                |> List.append (GameState.drawDeck startedGame)
                |> List.replicate (List.length hand.cards)
                |> List.map (List.countBy id
                    >> List.sortByDescending (fun (card, count) -> (count, card))
                    >> List.map fst)))
    let sortCardsByProbability =
        List.sortByDescending (fun candidate -> (candidate.probability, candidate.card))
        >> List.map (fun candidate -> candidate.card)

    startedGameOrError
    |> Result.map (candidateIdentities >> List.map (List.map sortCardsByProbability)) =! unrevealedCards

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Candidate identities are returned in descending order of probability`` (GameReadyToStart game) =
    let players = GameState.players game |> Set.toList
    let candidateProbabilities =
        Game.startGame game
        |> Result.map (candidateIdentities >> (List.map (List.map (List.map (fun candidate -> candidate.probability)))))
        |> Result.map (fun candidates ->
            candidates
            |> List.collect id
                >> List.map (fun candidate -> candidate.probability))))
    candidateProbabilities =! (candidateProbabilities |> Result.map (List.map (List.sortDescending)))
