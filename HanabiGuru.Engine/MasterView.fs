namespace HanabiGuru.Engine

type MasterView =
    {
        players : PlayerState list
        drawDeck : Card list
        fireworks : Card list
        discard : Card list
        fuseTokens : int
        clockTokens : int
        currentTurn : PlayerIdentity option
    }

module MasterView =

    let initial =
        {
            players = []
            drawDeck = []
            fireworks = []
            discard = []
            fuseTokens = 0
            clockTokens = 0
            currentTurn = None
        }

    let addPlayer view player = { view with players = PlayerState.create player :: view.players |> List.sort }

    let addFuseToken view = { view with fuseTokens = view.fuseTokens + 1 }

    let addClockToken view = { view with clockTokens = view.clockTokens + 1 }

    let addCardToDrawDeck view card = { view with drawDeck = card :: view.drawDeck }

    let dealCardToPlayer view card playerIdentity =
        let recipientPredicate player = player.identity = playerIdentity
        let recipients = List.filter recipientPredicate view.players
        let cardsPerRecipient = view.drawDeck |> List.filter ((=) card) |> List.truncate 1
        { view with
            drawDeck = List.removeEach (recipients |> List.map (fun _ -> card)) view.drawDeck
            players =
                List.update
                    recipientPredicate
                    (fun player -> { player with hand = cardsPerRecipient @ player.hand })
                    view.players
        }
