namespace HanabiGuru.Engine

type GameState =
    {
        masterView : MasterView
        playerViews : PlayerView list
    }

module GameState =
    
    let initial = { masterView = MasterView.initial; playerViews = [] }

    let apply applyToMasterView toEventForPlayer applyToPlayerView game event =
        let applyPlayerEvent view = 
            match toEventForPlayer view.self event with
            | Some playerEvent -> applyToPlayerView view playerEvent
            | None -> view
        let updatedExistingViews = List.map applyPlayerEvent game.playerViews
        
        let newPlayerViews =
            match event with
            | PlayerJoined player ->
                PlayerView.create player game.masterView :: updatedExistingViews |> List.sort
            | _ -> updatedExistingViews
        
        {
            masterView = applyToMasterView game.masterView event
            playerViews = newPlayerViews
        }
