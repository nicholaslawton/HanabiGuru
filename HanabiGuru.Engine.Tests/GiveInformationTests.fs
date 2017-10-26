module HanabiGuru.Engine.Tests.GiveInformationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``For each card in the recipients hand, all or none of the candidate identities must match the trait``
    (GameInProgress game)
    (cardTrait : CardTrait) =

    let recipient =
        GameState.activePlayer game
        |> Option.map (fun activePlayer ->
            GameState.playerView activePlayer game
            |> PlayerView.otherPlayers
            |> List.head)
        |> Option.get

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
let ``Information is given to the recipient only`` (GameInProgress game) (cardTrait : CardTrait) =
    let recipient =
        GameState.activePlayer game
        |> Option.map (fun activePlayer ->
            GameState.playerView activePlayer game
            |> PlayerView.otherPlayers
            |> List.head)
        |> Option.get

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
let ``Cannot give information which no cards match`` (GameInProgress game) =
    let (rank, hand) =
        GameState.hands game
        |> List.allPairs (List.map Rank [1..5])
        |> List.find (fun (rank, hand) ->
            not <| List.exists (fun { identity = (Card (_, r)) } -> rank = r) hand.cards)

    Game.giveInformation hand.player (RankTrait rank) game
    |> Result.mapError (select CannotGiveInformationReason.NoMatchingCards)
        =! Error [CannotGiveInformationReason.NoMatchingCards]
