module HanabiGuru.Engine.Tests.GiveInformationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open FsCheck

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Giving information costs a clock token``
    (GameInProgressAndGiveInformationTurn (game, (recipient, cardTrait))) =

    Game.giveInformation recipient cardTrait game
    |> Result.map (GameState.clockTokens) =! Ok (GameState.clockTokens game - 1)

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``For each card in the recipients hand, all or none of the candidate identities must match the trait``
    (GameInProgressAndGiveInformationTurn (game, (recipient, cardTrait))) =

    let candidateTraits =
        Game.giveInformation recipient cardTrait game
        |> Result.map (fun game ->
            let view = GameState.playerView recipient game
            PlayerView.hand view
            |> List.map (PlayerView.CardIdentity.deduce view)
            |> List.map (List.map (fun { card = Card (suit, rank) } ->
                match cardTrait with
                | SuitTrait _ -> SuitTrait suit
                | RankTrait _ -> RankTrait rank) >> List.distinct))
        |> function
            | Error error -> failwith (sprintf "%A" error)
            | Ok candidateTraits -> candidateTraits

    test <@ candidateTraits
        |> List.forall (fun traits -> traits = [cardTrait] || not (List.contains cardTrait traits)) @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Information is given to the recipient only``
    (GameInProgressAndGiveInformationTurn (game, (recipient, cardTrait))) =

    let bystandersDeductions game =
        GameState.players game
        |> List.filter ((<>) recipient)
        |> List.map (fun player ->
            let view = GameState.playerView player game
            PlayerView.hand view
            |> List.map (PlayerView.CardIdentity.deduce view))

    Game.giveInformation recipient cardTrait game
    |> Result.map bystandersDeductions =! Ok (bystandersDeductions game)

let private select reason = function
    | CannotGiveInformation reasons -> List.filter ((=) reason) reasons
    | _ -> []

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Giving information becomes impossible once the clock tokens are exhausted``
    (GameInProgressAndGiveInformationTurn (game, (recipient, cardTrait))) =

    Game.giveInformation recipient cardTrait
    |> List.replicate (GameState.clockTokens game + 1)
    |> List.fold GameAction.perform (Ok game)
    |> Result.mapError (select CannotGiveInformationReason.NoClockTokensAvailable)
        =! Error [CannotGiveInformationReason.NoClockTokensAvailable]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot give information to another player which no cards match`` (GameInProgress game) =
    let traitAndHand =
        GameState.hands game
        |> List.filter (fun hand -> Some hand.player <> GameState.activePlayer game)
        |> List.allPairs (List.map (Rank >> RankTrait) [1..5] @ List.map SuitTrait [Blue; Green; Red; Yellow; White])
        |> List.tryFind (function
            | (SuitTrait suit, hand) ->
                not <| List.exists (fun { identity = (Card (s, _)) } -> s = suit) hand.cards
            | (RankTrait rank, hand) ->
                not <| List.exists (fun { identity = (Card (_, r)) } -> r = rank) hand.cards)

    traitAndHand <> None ==> lazy

    let (cardTrait, { player = recipient }) = traitAndHand |> Option.get
    Game.giveInformation recipient cardTrait game
    |> Result.mapError (select CannotGiveInformationReason.NoMatchingCards)
        =! Error [CannotGiveInformationReason.NoMatchingCards]

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot give information to self`` (GameInProgress game) (cardTrait : CardTrait) =
    Game.giveInformation (GameState.activePlayer game |> Option.get) cardTrait game
        =! Error (CannotGiveInformation [CannotGiveInformationReason.InvalidRecipient])

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Cannot give information to a player who has not joined the game``
    (GameInProgress game)
    (recipient : PlayerIdentity)
    (cardTrait : CardTrait) =

    not <| List.contains recipient (GameState.players game) ==> lazy

    Game.giveInformation recipient cardTrait game
    |> Result.mapError (select CannotGiveInformationReason.InvalidRecipient)
        =! Error [CannotGiveInformationReason.InvalidRecipient]
