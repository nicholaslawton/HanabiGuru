namespace HanabiGuru.Engine

type GameState =
    {
        masterView : MasterView
        playerViews : PlayerView list
    }

module GameState =
    
    let initial = { masterView = MasterView.initial; playerViews = [] }

    let apply game = function
        | PlayerJoined player as event ->
            let applyPlayerEvent view = 
                match GameEvent.toEventForPlayer view.self event with
                | Some playerEvent -> PlayerEvent.apply view playerEvent
                | None -> view
            let newPlayerView = PlayerView.createWithOthers player game.masterView.players
            let updatedExistingViews = List.map applyPlayerEvent game.playerViews

            { game with
                masterView = GameEvent.apply game.masterView event
                playerViews = newPlayerView :: updatedExistingViews |> List.sort
            }
        | CardAddedToDrawDeck _ -> game
