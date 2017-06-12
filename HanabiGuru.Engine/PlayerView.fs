namespace HanabiGuru.Engine

type PlayerView = { self : Player; otherPlayers : Player list }

module PlayerView =

    let sortOtherPlayersInRelativeTurnOrder view =
        let following, preceding = List.partition ((<) view.self) view.otherPlayers
        let others = List.sort following @ List.sort preceding
        { view with otherPlayers = others }

    let createWithOthers self otherPlayers =
        { self = self; otherPlayers = otherPlayers }
        |> sortOtherPlayersInRelativeTurnOrder

    let create self = createWithOthers self []

    let addOtherPlayer view player =
        { view with otherPlayers = player :: view.otherPlayers }
        |> sortOtherPlayersInRelativeTurnOrder
