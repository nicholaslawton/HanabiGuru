namespace HanabiGuru.Engine

type MasterView =
    {
        players : Player list
        drawDeck : Card list
    }

module MasterView =

    let initial = { players = []; drawDeck = [] }

    let addPlayer view player = { view with players = player :: view.players |> List.sort }

    let addCardToDrawDeck view card = { view with drawDeck = card :: view.drawDeck }
