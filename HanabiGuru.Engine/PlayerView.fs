namespace HanabiGuru.Engine

type PlayerView =
    {
        self : PlayerState
        otherPlayers : PlayerState list
        drawDeckSize : int
        fuseTokens : int
        clockTokens : int
    }

module PlayerView =

    let sortOtherPlayersInRelativeTurnOrder view =
        let following, preceding = List.partition ((<) view.self) view.otherPlayers
        let others = List.sort following @ List.sort preceding
        { view with otherPlayers = others }

    let create self masterView =
        {
            self = PlayerState.create self
            otherPlayers = masterView.players
            drawDeckSize = List.length masterView.drawDeck
            fuseTokens = 0
            clockTokens = 0
        }
        |> sortOtherPlayersInRelativeTurnOrder

    let addOtherPlayer view player =
        { view with otherPlayers = PlayerState.create player :: view.otherPlayers }
        |> sortOtherPlayersInRelativeTurnOrder

    let addCardToDrawDeck view = { view with drawDeckSize = view.drawDeckSize + 1 }

    let dealCardToSelf view =
        { view with
            drawDeckSize = view.drawDeckSize - 1
            self = { view.self with hand = ConcealedCard :: view.self.hand }
        }

    let dealCardToOtherPlayer view card playerIdentity =
        let recipientPredicate player = player.identity = playerIdentity
        { view with
            drawDeckSize = view.drawDeckSize - (view.otherPlayers |> List.filter recipientPredicate |> List.length)
            otherPlayers =
                List.update
                    recipientPredicate
                    (fun player -> { player with hand = card :: player.hand })
                    view.otherPlayers
        }
