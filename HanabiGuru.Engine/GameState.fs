namespace HanabiGuru.Engine

type GameState =
    {
        masterView : MasterView
        playerViews : PlayerView list
    }

module GameState =
    
    let initial = { masterView = MasterView.initial; playerViews = [] }

    let apply game event =
        let applyPlayerEvent view = 
            match GameEvent.toEventForPlayer view.self.identity event with
            | Some playerEvent -> PlayerEvent.apply view playerEvent
            | None -> view
        let updatedExistingViews = List.map applyPlayerEvent game.playerViews
        
        let newPlayerViews =
            match event with
            | PlayerJoined player ->
                PlayerView.create player game.masterView :: updatedExistingViews |> List.sort
            | _ -> updatedExistingViews
        
        {
            masterView = GameEvent.apply game.masterView event
            playerViews = newPlayerViews
        }
     
    let players = 
        EventHistory.choose (function 
            | PlayerJoined player -> Some player 
            | _ -> None) 
        >> set 
