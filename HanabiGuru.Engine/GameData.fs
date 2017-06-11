namespace HanabiGuru.Engine

type GameData =
    {
        state : GameState
    }

module GameData =
    
    let initial = { state = GameState.initial }

    let processEvent game event =
        { state = GameEvent.processEvent game.state event }
