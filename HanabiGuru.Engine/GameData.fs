namespace HanabiGuru.Engine

type GameData =
    {
        state : GameState
        views : PlayerView list
    }

module GameData =
    
    let initial = { state = GameState.initial; views = [] }

    let processEvent game ((PlayerJoined player) as event) =
        let applyEventToView view = 
            GameEvent.toEventForPlayer view.self event
            |> function
                | Some playerEvent -> PlayerEvent.processEvent view playerEvent
                | None -> view
        let newPlayerView = PlayerView.createWithOthers player game.state.players
        let updatedExistingViews = List.map applyEventToView game.views

        { game with
            state = GameEvent.processEvent game.state event
            views = newPlayerView :: updatedExistingViews |> List.sort
        }
