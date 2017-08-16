namespace HanabiGuru.Engine

type PlayerIdentity = { name : string }

type PlayerHand =
    {
        player : PlayerIdentity
        cards : Card list
    }

module PlayerIdentity =

    let create name = { name = name }

module PlayerHand =
    
    let create player cards =
        {
            player = player
            cards = cards
        }
