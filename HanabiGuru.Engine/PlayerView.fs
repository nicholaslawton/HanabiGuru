namespace HanabiGuru.Engine

type PlayerView = { self : Player; otherPlayers : Player list }

module PlayerView =

    let create self = { self = self; otherPlayers = [] }

    let sortOtherPlayersInRelativeTurnOrder view =
        let following, preceding = List.partition ((<) view.self) view.otherPlayers
        let others = List.sort following @ List.sort preceding
        { view with otherPlayers = others }

    let addOtherPlayer view player =
        { view with otherPlayers = player :: view.otherPlayers }
        |> sortOtherPlayersInRelativeTurnOrder
