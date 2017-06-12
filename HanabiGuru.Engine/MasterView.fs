namespace HanabiGuru.Engine

type MasterView = { players : Player list }

module MasterView =

    let initial = { players = [] }

    let addPlayer view player = { players = player :: view.players |> List.sort }
