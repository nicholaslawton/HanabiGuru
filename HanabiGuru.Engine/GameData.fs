namespace HanabiGuru.Engine

type GameData =
    {
        masterView : MasterView
        playerViews : PlayerView list
    }

module GameData =
    
    let initial = { masterView = MasterView.initial; playerViews = [] }

    let apply game ((PlayerJoined player) as event) =
        let applyEventToView view = 
            GameEvent.toEventForPlayer view.self event
            |> function
                | Some playerEvent -> PlayerEvent.apply view playerEvent
                | None -> view
        let newPlayerView = PlayerView.createWithOthers player game.masterView.players
        let updatedExistingViews = List.map applyEventToView game.playerViews

        { game with
            masterView = GameEvent.apply game.masterView event
            playerViews = newPlayerView :: updatedExistingViews |> List.sort
        }
