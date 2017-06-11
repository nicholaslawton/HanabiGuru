namespace HanabiGuru.Client.Console

open HanabiGuru.Engine

type GameData =
    {
        state : GameState
    }

module GameData =
    
    let initial = { state = GameState.initial }
