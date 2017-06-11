namespace HanabiGuru.Engine

type GameState = { players : Player list }

module GameState =

    let initial = { players = [] }

    let addPlayer state player = { players = player :: state.players |> List.sort }
