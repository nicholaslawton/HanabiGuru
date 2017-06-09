module HanabiGuru.Engine.Tests.PlayersJoiningIntegrationTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

let private addPlayer history player =
    let newHistory, result = Game.addPlayer EventHistory.recordEvent Game.canAddPlayer history player
    result =! PlayerAdded
    newHistory

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``All players which join the game are added to the game state in turn order`` (Players players) =
    let history = List.fold addPlayer EventHistory.empty players
    let events = EventHistory.allEvents history
    let state = List.fold GameEvent.processEvent GameState.initial events
    state.players =! List.sort players

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``The player sees all other players that joined the game`` (OneOrMorePlayers (self, others)) =
    let history = List.fold addPlayer EventHistory.empty (self :: others)
    let events = EventHistory.allEvents history |> List.choose (GameEvent.toEventForPlayer self)
    let view = List.fold PlayerEvent.processEvent (PlayerView.create self) events
    view.self =! self
    List.sort view.otherPlayers =! List.sort others

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``Each player has a different player following them in turn order``
    (TwoOrMorePlayers players) =

    let history = List.fold addPlayer EventHistory.empty players
    let events = EventHistory.allEvents history
    let eventsForPlayer player = List.choose (GameEvent.toEventForPlayer player) events
    let views =
        players
        |> List.map PlayerView.create
        |> List.map (fun view -> List.fold PlayerEvent.processEvent view (eventsForPlayer view.self))
    views |> List.map (fun view -> view.otherPlayers |> List.head) |> List.sort =! List.sort players

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``Each player has a different player preceding them in turn order``
    (TwoOrMorePlayers players) =

    let history = List.fold addPlayer EventHistory.empty players
    let events = EventHistory.allEvents history
    let eventsForPlayer player = List.choose (GameEvent.toEventForPlayer player) events
    let views =
        players
        |> List.map PlayerView.create
        |> List.map (fun view -> List.fold PlayerEvent.processEvent view (eventsForPlayer view.self))
    views |> List.map (fun view -> view.otherPlayers |> List.last) |> List.sort =! List.sort players
