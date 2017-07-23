module HanabiGuru.Engine.Tests.GameStateIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private applyEvent = GameState.apply GameEvent.apply GameEvent.toEventForPlayer PlayerEvent.apply

[<Property>]
let ``All players added to the game are added to the master view`` (players : PlayerIdentity list) =
    players
    |> List.map PlayerJoined
    |> List.fold applyEvent GameState.initial
    |> fun game -> game.masterView.players
    |> List.map (fun player -> player.identity)
    |> List.sort =! List.sort players

[<Property>]
let ``A view is created for each player added to the game`` (players : PlayerIdentity list) =
    players
    |> List.map PlayerJoined
    |> List.fold applyEvent GameState.initial
    |> fun game -> game.playerViews
    |> List.map (fun view -> view.self.identity)
    |> List.sort =! List.sort players

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>]
let ``All players added to the game appear in all views`` (Players players) =
    players
    |> List.map PlayerJoined
    |> List.fold applyEvent GameState.initial
    |> fun game -> game.playerViews
    |> List.map (fun view -> view.self.identity :: List.map (fun player -> player.identity) view.otherPlayers)
    |> List.map List.sort =! List.replicate (List.length players) (List.sort players)

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

[<Property>]
let ``All players see the size of the draw deck`` (state : GameState) (cards : Card list) =
    let game = { state with playerViews = List.map (fun view -> { view with drawDeckSize = 0 }) state.playerViews }

    cards
    |> List.map GameEvent.CardAddedToDrawDeck
    |> List.fold applyEvent game
    |> fun game -> game.playerViews
    |> List.map (fun view -> view.drawDeckSize) =! List.replicate (List.length state.playerViews) (List.length cards)

[<Property>]
let ``The order in which players and cards are added does not affect the result, ignoring draw deck order``
    (Players players)
    (cards : Card list) =

    let events = List.map PlayerJoined players @ List.map GameEvent.CardAddedToDrawDeck cards

    let ignoreDrawDeckOrder state =
        { state with
            masterView = { state.masterView with drawDeck = List.sort state.masterView.drawDeck }
        }
    let addEvents transformation =
        transformation
        >> List.fold applyEvent GameState.initial
        >> ignoreDrawDeckOrder

    addEvents id events =! addEvents List.rev events
    addEvents id events =! addEvents List.sort events
    addEvents List.rev events =! addEvents List.sort events
