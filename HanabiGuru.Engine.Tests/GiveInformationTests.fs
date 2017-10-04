module HanabiGuru.Engine.Tests.GiveInformationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``For each card in the recipients hand, all or none of the candidate identities must match the trait``
    (GameInProgress game)
    (suit : Suit) =

    let recipient =
        GameState.activePlayer game
        |> Option.map (fun activePlayer ->
            GameState.playerView activePlayer game
            |> PlayerView.otherPlayers
            |> List.head)
        |> Option.get

    let candidateSuits =
        Game.giveInformation recipient suit game
        |> Result.map (fun game ->
            let view = GameState.playerView recipient game
            PlayerView.hand view
            |> List.map (PlayerView.CardIdentity.deduce view)
            |> List.map (List.map (fun { card = Card (suit, _) } -> suit) >> List.distinct))
        |> function
            | Error error -> failwith (sprintf "%A" error)
            | Ok candidateSuits -> candidateSuits

    test <@ candidateSuits |> List.forall (fun suits -> suits = [suit] || not (List.contains suit suits)) @>

[<Property(Arbitrary = [| typeof<GameGeneration> |])>]
let ``Information is given to the recipient only`` (GameInProgress game) (suit : Suit) =
    let recipient =
        GameState.activePlayer game
        |> Option.map (fun activePlayer ->
            GameState.playerView activePlayer game
            |> PlayerView.otherPlayers
            |> List.head)
        |> Option.get

    let bystandersDeductions game =
        GameState.players game
        |> Set.toList
        |> List.filter ((<>) recipient)
        |> List.map (fun player ->
            let view = GameState.playerView player game
            PlayerView.hand view
            |> List.map (PlayerView.CardIdentity.deduce view))

    Game.giveInformation recipient suit game
    |> Result.map bystandersDeductions =! Ok (bystandersDeductions game)
