namespace HanabiGuru.Engine

type PlayerView =
    {
        self : Player
        otherPlayers : Player list
        drawDeckSize : int
    }

module PlayerView =

    let sortOtherPlayersInRelativeTurnOrder view =
        let following, preceding = List.partition ((<) view.self) view.otherPlayers
        let others = List.sort following @ List.sort preceding
        { view with otherPlayers = others }

    let create self masterView =
        { self = self; otherPlayers = masterView.players; drawDeckSize = List.length masterView.drawDeck }
        |> sortOtherPlayersInRelativeTurnOrder

    let addOtherPlayer view player =
        { view with otherPlayers = player :: view.otherPlayers }
        |> sortOtherPlayersInRelativeTurnOrder

    let addCardToDrawDeck view = { view with drawDeckSize = view.drawDeckSize + 1 }
