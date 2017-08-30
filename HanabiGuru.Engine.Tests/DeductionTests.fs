module HanabiGuru.Engine.Tests.DeductionTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``There is always at least one candidate identity for all cards`` (GameReadyToStart game) =
    let players = GameState.players game |> Set.toList

    Game.startGame game
    |> Result.map (fun game -> List.map (fun player -> GameState.playerView player game) players)
    |> Result.map (List.map (fun view -> (PlayerView.hand view, view)))
    |> Result.map (List.collect (fun (cards, view) -> List.map (PlayerView.CardIdentity.deduce view) cards))
    |> Result.map (List.filter List.isEmpty) =! Ok []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``All candidate identities always have a probability above zero and no greater than one`` (GameReadyToStart game) =
    let players = GameState.players game |> Set.toList

    Game.startGame game
    |> Result.map (fun game -> List.map (fun player -> GameState.playerView player game) players)
    |> Result.map (List.map (fun view -> (PlayerView.hand view, view)))
    |> Result.map (List.collect (fun (cards, view) -> List.map (PlayerView.CardIdentity.deduce view) cards))
    |> Result.map (List.collect id)
    |> Result.map (List.filter (fun candidate -> candidate.probability <= 0.0 || candidate.probability > 1.0)) =! Ok []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The sum of probabilities for all candidates for each card is one`` (GameReadyToStart game) =
    let players = GameState.players game |> Set.toList

    test <@ Game.startGame game
        |> Result.map (fun game -> List.map (fun player -> GameState.playerView player game) players)
        |> Result.map (List.map (fun view -> (PlayerView.hand view, view)))
        |> Result.map (List.map (fun (cards, view) -> List.map (PlayerView.CardIdentity.deduce view) cards))
        |> Result.map (List.map (List.map (List.sumBy (fun candidate -> candidate.probability))))
        |> Result.map (List.collect id)
        |> Result.map (List.map ((-) 1.0))
        |> Result.map (List.map abs)
        |> Result.map (List.forall ((>) 1e-10)) = Ok true
    @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Candidate identities include all identities for which at least one card remains unrevealed``
    (GameReadyToStart game) =

    let startedGameOrError = Game.startGame game
    let players = GameState.players game |> Set.toList
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
    |> Result.map (fun game ->
        players
        |> List.map (fun player ->
            let view = GameState.playerView player game
            view
            |> PlayerView.hand
            |> List.map (fun card -> PlayerView.CardIdentity.deduce view card)
            |> List.map (List.map (fun candidate -> candidate.card) >> set))) =! unrevealedCards

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``The probabilities of candidate identities are relative to the number of unrevealed instances``
    (GameReadyToStart game) =

    let startedGameOrError = Game.startGame game
    let players = GameState.players game |> Set.toList
    let unrevealedCards =
        startedGameOrError
        |> Result.map (fun startedGame ->
            GameState.hands startedGame
            |> List.map (fun hand ->
                    hand.cards
                    |> List.append (GameState.drawDeck startedGame)
                    |> List.replicate (List.length hand.cards)
                    |> List.map (List.countBy id)
                    |> List.map (List.sortByDescending (fun (card, count) -> (count, card)))
                    |> List.map (List.map fst)))

    startedGameOrError
    |> Result.map (fun game ->
        players
        |> List.map (fun player ->
            let view = GameState.playerView player game
            view
            |> PlayerView.hand
            |> List.map (fun card -> PlayerView.CardIdentity.deduce view card)
            |> List.map (List.sortByDescending (fun candidate -> (candidate.probability, candidate.card)))
            |> List.map (List.map (fun candidate -> candidate.card)))) =! unrevealedCards

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Candidate identities are returned in reverse order of probability`` (GameReadyToStart game) =
    let players = GameState.players game |> Set.toList
    let candidateProbabilities =
        Game.startGame game
        |> Result.map (fun game -> List.map (fun player -> GameState.playerView player game) players)
        |> Result.map (List.map (fun view -> (PlayerView.hand view, view)))
        |> Result.map (List.collect (fun (cards, view) -> List.map (PlayerView.CardIdentity.deduce view) cards))
        |> Result.map (List.map (List.map (fun candidate -> candidate.probability)))
    candidateProbabilities =! (candidateProbabilities |> Result.map (List.map (List.sortDescending)))
