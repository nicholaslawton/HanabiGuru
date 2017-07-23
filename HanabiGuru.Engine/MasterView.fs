namespace HanabiGuru.Engine

type MasterView =
    {
        players : PlayerState list
        drawDeck : Card list
        fireworks : Card list
        discard : Card list
    }

module MasterView =

    let initial =
        {
            players = []
            drawDeck = []
            fireworks = []
            discard = []
        }

    let addPlayer view player = { view with players = PlayerState.create player :: view.players |> List.sort }

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
