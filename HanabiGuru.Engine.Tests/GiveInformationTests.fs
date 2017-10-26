module HanabiGuru.Engine.Tests.GiveInformationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine
open FsCheck

let private legalAction recipientIndex cardIndex cardTrait game =
    GameState.activePlayer game
    |> Option.map (fun activePlayer ->
        let view = GameState.playerView activePlayer game
        let others = PlayerView.otherPlayers view
        let recipient = List.item (recipientIndex % List.length others) others
        let cardTrait =
            let cards = (PlayerView.otherHand recipient view).cards
            let card = List.item (cardIndex % List.length cards) cards
            match (cardTrait, card) with
            | (SuitTrait _, { identity = Card (suit, _) }) -> SuitTrait suit
            | (RankTrait _, { identity = Card (_, rank) }) -> RankTrait rank
        (recipient, cardTrait))
    |> Option.get


[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``For each card in the recipients hand, all or none of the candidate identities must match the trait``
    (GameInProgress game)
    (PositiveInt recipientIndex)
    (PositiveInt cardIndex)
    (cardTrait : CardTrait) =

    let (recipient, cardTrait) = legalAction recipientIndex cardIndex cardTrait game

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
    (GameInProgress game)
    (PositiveInt recipientIndex)
    (PositiveInt cardIndex)
    (cardTrait : CardTrait) =
    
    let (recipient, cardTrait) = legalAction recipientIndex cardIndex cardTrait game

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
