module HanabiGuru.Engine.Tests.GameStateTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``Applying an event updates all views``
    (game : GameState)
    (event : GameEvent)
    (applyToMasterView : (MasterView -> GameEvent -> MasterView))
    (toPlayerEvent : (Player -> GameEvent -> PlayerEvent))
    (applyToPlayerView : (PlayerView -> PlayerEvent -> PlayerView)) =

    let toEventForPlayer player = toPlayerEvent player >> Some
    let updatedGame = GameState.apply applyToMasterView toEventForPlayer applyToPlayerView game event

    updatedGame.masterView =! applyToMasterView game.masterView event

    let updatePlayerView view = toPlayerEvent view.self event |> applyToPlayerView view
    let updatedPlayerViews = List.map updatePlayerView game.playerViews
    test <@ updatedPlayerViews |> List.forall (fun view -> List.contains view updatedGame.playerViews) @>
