namespace HanabiGuru.Engine

open Model

type GameState = { players : Player list }

module GameState =

    let playerJoined state player = { players = player :: state.players }
