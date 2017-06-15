module HanabiGuru.Engine.Tests.GameStateIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private applyEvent = GameState.apply GameEvent.apply GameEvent.toEventForPlayer PlayerEvent.apply

[<Property>]
let ``All players added to the game are added to the master view`` (players : Player list) =
    players
    |> List.map PlayerJoined
    |> List.fold applyEvent GameState.initial
    |> fun game -> game.masterView.players
    |> List.sort =! List.sort players

[<Property>]
let ``A view is created for each player added to the game`` (players : Player list) =
    players
    |> List.map PlayerJoined
    |> List.fold applyEvent GameState.initial
    |> fun game -> game.playerViews
    |> List.map (fun view -> view.self)
    |> List.sort =! List.sort players

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``All players added to the game appear in all views`` (Players players) =
    players
    |> List.map PlayerJoined
    |> List.fold applyEvent GameState.initial
    |> fun game -> game.playerViews
    |> List.map (fun view -> view.self :: view.otherPlayers)
    |> List.map List.sort
        =! List.replicate (List.length players) (List.sort players)

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``The order in which players are added does not affect the result`` (Players players) =
    let addPlayers transformation =
        transformation
        >> List.map PlayerJoined
        >> List.fold applyEvent GameState.initial

    addPlayers id players =! addPlayers List.rev players
    addPlayers id players =! addPlayers List.sort players
    addPlayers List.rev players =! addPlayers List.sort players

[<Property>]
let ``All cards added to the draw deck are added to the master view`` (cards : Card list) =
    cards
    |> List.map GameEvent.CardAddedToDrawDeck
    |> List.fold applyEvent GameState.initial
    |> fun game -> game.masterView.drawDeck
    |> List.sort =! List.sort cards
