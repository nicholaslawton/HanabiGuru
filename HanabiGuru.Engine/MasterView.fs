namespace HanabiGuru.Engine

type MasterView =
    {
        players : Player list
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

    let addPlayer view player = { view with players = player :: view.players |> List.sort }

    let addCardToDrawDeck view card = { view with drawDeck = card :: view.drawDeck }

    let dealCardToPlayer view card playerIdentity =
        match List.tryFindIndex ((=) card) view.drawDeck,
            List.tryFindIndex (fun player -> player.identity = playerIdentity) view.players with
        | Some cardIndex, Some recipientIndex ->
            let addCardToHand card player = { player with hand = card :: player.hand }
            let deal index player = if index = recipientIndex then addCardToHand card player else player
            { view with
                players = List.mapi deal view.players
                drawDeck = List.indexed view.drawDeck |> List.filter (fst >> ((<>) cardIndex)) |> List.map snd
            }
        | _ -> view
