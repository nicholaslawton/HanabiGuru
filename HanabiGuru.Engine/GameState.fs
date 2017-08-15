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

    let fuseTokens _ = GameRules.fuseTokensAvailable

    let clockTokens _ = GameRules.clockTokensAvailable

    let drawDeck game =
        let cardsAddedToDrawDeck = 
            game
            |> EventHistory.choose (function
                | CardAddedToDrawDeck card -> Some card
                | _ -> None)
        let cardsDealt = 
            game
            |> EventHistory.choose (function
                | CardDealtToPlayer (card, _) -> Some card
                | _ -> None)
        List.removeEach cardsDealt cardsAddedToDrawDeck

    let hands game =
        game
        |> EventHistory.choose (function
            | CardDealtToPlayer (card, player) -> Some (card, player)
            | _ -> None)
        |> List.groupBy snd
        |> List.map (Pair.mapSnd (List.map fst))
        |> List.map (fun (player, cards) -> { identity = player; hand = cards })

    let activePlayer = players >> Set.toList >> List.sort >> List.tryHead

    let playerView player = EventHistory.choose (GameEvent.toEventForPlayer player)
